import numpy as np
from random import random
from sklearn.model_selection import train_test_split
import json
import os
import math
import csv
import librosa

# save activation and derivatives
# implement back propagation
# implement gradient descent
# implement train
# train our net with some dummy dataset
# make some predictions

## Preparing dataset

dataset_path = "E:/ProjetInge/audio_brute"
json_path = "data_melspec.json"
chat_path = "C:/Users/HP/Documents/2020-2021/PROJET-INGE"
n_fft = 2048 # interval we consider to apply FFT
sr = 22050 # sample rate
n_mels = 20 # nb of mel bands
hop_length = 512 # sliding window for FFT

data = {
    "inputs": [],
    "outputs": []
}

with open(chat_path+"/chat.csv", newline='') as csvfile:
    chat = csv.reader(csvfile, delimiter=';')
    i = 0
    for row in chat:
        i += 1
        if i > 1:
            data['outputs'].append([int(row[4]),int(row[5])])

            # charge audio file
            scale_file = dataset_path+"/chat_"+str(i-1)+".wav"
            scale, sr = librosa.load(scale_file)

            # Mel filter banks
            filter_banks = librosa.filters.mel(n_fft = n_fft, sr = sr, n_mels = n_mels)

            # Mel spectro
            mel_spectrogram = librosa.feature.melspectrogram(scale, sr = sr, n_fft = n_fft,
                                                            hop_length = hop_length, n_mels = n_mels)
            log_mel_spectrogram = librosa.power_to_db(mel_spectrogram)

            # adding to items
            data["inputs"].append(log_mel_spectrogram)


## Definition de la classe MLP

class MLP(object):
    """A Multilayer Perceptron class.
    """

    def __init__(self, num_inputs=3, hidden_layers=[3, 3], num_outputs=2):
        """Constructor for the MLP. Takes the number of inputs,
            a variable number of hidden layers, and number of outputs
        Args:
            num_inputs (int): Number of inputs
            hidden_layers (list): A list of ints for the hidden layers
            num_outputs (int): Number of outputs
        """

        self.num_inputs = num_inputs
        self.hidden_layers = hidden_layers
        self.num_outputs = num_outputs

        # create a generic representation of the layers
        layers = [num_inputs] + hidden_layers + [num_outputs]

        # create random connection weights for the layers
        weights = []
        for i in range(len(layers) - 1):
            w = np.random.rand(layers[i], layers[i + 1])
            weights.append(w)
        self.weights = weights

        # NEW FOR NET

        # save derivatives per layer
        derivatives = []
        for i in range(len(layers) - 1): # as many derivatives as weights
            d = np.zeros((layers[i], layers[i + 1])) # matrix
            derivatives.append(d)
        self.derivatives = derivatives

        # save activations per layer
        activations = [] # list of array which represent activation for a given layer
        for i in range(len(layers)):
            a = np.zeros(layers[i])
            activations.append(a)
        self.activations = activations


    def forward_propagate(self, inputs):
        """Computes forward propagation of the network based on input signals.
        Args:
            inputs (ndarray): Input signals
        Returns:
            activations (ndarray): Output values
        """

        # the input layer activation is just the input itself
        activations = inputs # inputs because 1st layer

        # save the activations for backpropogation
        self.activations[0] = activations

        # iterate through the network layers
        for i, w in enumerate(self.weights):
            # calculate matrix multiplication between previous activation and weight matrix
            net_inputs = np.dot(activations, w)

            # apply sigmoid activation function
            activations = self._sigmoid(net_inputs)

            # save the activations for backpropogation
            self.activations[i + 1] = activations
            # i+1 because:
            # a_3 = s(h_3)
            # h_3 = a_2*W_2

        # return output layer activation
        return activations


    def back_propagate(self, error):
        """Backpropogates an error signal.
        Args:
            error (ndarray): The error to backprop.
        Returns:
            error (ndarray): The final error of the input
        """
        # dE/dW_i = (y - a_[i+1]) s'(h_[i+1])) a_i
        # s'(h_[i+1]) = s(h_[i+1])(1 - s(h_[i+1])
        # s(h_[i+1]) = a_[i+1]

        # dE/dW_[i-1] = (y - a_[i+1]) s'(h_[i+1])) W_i s'(h_i) a_[i-1]

        # iterate backwards through the network layers
        for i in reversed(range(len(self.derivatives))): # from right to left

            # get activation for previous layer
            activations = self.activations[i+1]

            # apply sigmoid derivative function ((y - a_[i+1]) s'(h_[i+1])))
            delta = error * self._sigmoid_derivative(activations)

            # reshape delta as to have it as a 2d array
            # ex: ndarray([0.1,0.2]) -> ndarray([[0.1,0.2]])
            delta_re = delta.reshape(delta.shape[0], -1).T # .T = transposed

            # get activations for current layer
            current_activations = self.activations[i]

            # reshape activations as to have them as a 2d column matrix
            # ex: ndarray([0.1,0.2]) -> ndarray([[0.1],[0.2]]) (vertical vector)
            current_activations = current_activations.reshape(current_activations.shape[0],-1)

            # save derivative after applying matrix multiplication
            self.derivatives[i] = np.dot(current_activations, delta_re)

            # backpropogate the next error
            # (y - a_[i+1]) s'(h_[i+1])) W_i
            error = np.dot(delta, self.weights[i].T)

        # return(error)


    def train(self, inputs, targets, epochs, learning_rate):
        """Trains model running forward prop and backprop
        Args:
            inputs (ndarray): X
            targets (ndarray): Y
            epochs (int): Num. epochs we want to train the network for
            learning_rate (float): Step to apply to gradient descent
        """
        # now enter the training loop
        for i in range(epochs):
            sum_errors = 0

            # iterate through all the training data
            for j, input in enumerate(inputs):
                target = targets[j]

                # activate the network!
                output = self.forward_propagate(input)

                error = target - output

                self.back_propagate(error)

                # now perform gradient descent on the derivatives
                # (this will update the weights
                self.gradient_descent(learning_rate)

                # keep track of the MSE for reporting later
                sum_errors += self._mse(target, output)

            # Epoch complete, report the training error
            print("Error: {} at epoch {}".format(sum_errors / len(items), i+1))

        print("Training complete!")
        print("=====")


    def gradient_descent(self, learningRate=1):
        """Learns by descending the gradient
        Args:
            learningRate (float): How fast to learn.
        """
        # update the weights by stepping down the gradient
        for i in range(len(self.weights)):
            weights = self.weights[i]
            derivatives = self.derivatives[i]
            weights += derivatives * learningRate


    def _sigmoid(self, x):
        """Sigmoid activation function
        Args:
            x (float): Value to be processed
        Returns:
            y (float): Output
        """

        y = 1.0 / (1 + np.exp(-x))
        return y


    def _sigmoid_derivative(self, x):
        """Sigmoid derivative function
        Args:
            x (float): Value to be processed
        Returns:
            y (float): Output
        """
        return x * (1.0 - x)


    def _mse(self, target, output):
        """Mean Squared Error loss function
        Args:
            target (ndarray): The ground trut
            output (ndarray): The predicted values
        Returns:
            (float): Output
        """
        return np.average((target - output) ** 2)

## SCRIPT

X = np.array(data["inputs"])
y = np.array(data["outputs"])

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3)

for i in range(len(y)):
    print(y[i].shape)

print(len for i in range(len(data['inputs']))

# create an mlp
# create a dummy data
# forward propagation
# back propagation

# create a Multilayer Perceptron with one hidden layer (with 5 neurones)
mlp = MLP(20, [100], 2)

# train network
mlp.train(X_train, y_train, 50, 0.1)

# get a prediction
output = mlp.forward_propagate(X_test)

print()
print("Our network believes that {} has {} bk and {} bit".format(X_test[0], output[0], output[1]))