import numpy as np

class MLP:

    def __init__(self, num_inputs = 3, num_hidden = [3, 5], num_outputs = 2):
        """Constructor for the MLP. Takes the number of inputs,
            a variable number of hidden layers, and number of outputs
        Args:
            num_inputs (int): Number of inputs
            hidden_layers (list): A list of ints for the hidden layers
            num_outputs (int): Number of outputs
        """

        self.num_inputs = num_inputs
        self.num_hidden = num_hidden
        self.num_outputs = num_outputs

        # interior representation of the network
        # list where each element of the list represent the number of neurones
        # in the layer
        layers = [self.num_inputs] + self.num_hidden + [self.num_outputs]

        # initiate random weights
        self.weights = []
        for i in range(len(layers)-1):
            w = np.random.rand(layers[i], layers[i+1]) # weight matrix
            # (nb of neurones in the actual layer, nb of neurones in the next one)
            self.weights.append(w) # list of weight matrix, as many weight matrix
            # as layers minus one (intervalle pas poteaux)

    def forward_propagate(self, inputs):
        """Computes forward propagation of the network based on input signals.
        Args:
            inputs (ndarray): Input signals
        Returns:
            activations (ndarray): Output values
        """

        activations = inputs

        for w in self.weights:

            # calculate net inputs
            net_inputs = np.dot(activations, w) # matrix multiplication
            # h = xW

            # calculate activations
            activations = self._sigmoid(net_inputs) # fonction d'activation
            # sigmoide

        return activations

    def _sigmoid(self, x):
        """Sigmoid activation function
        Args:
            x (float): Value to be processed
        Returns:
            y (float): Output
        """

        return 1 / (1 + np.exp(-x))

if __name__ == "__main__":

    # create an MLP
    mlp = MLP() # taking default values

    # create some inputs
    inputs = np.random.rand(mlp.num_inputs)

    # perform forward prop
    outputs = mlp.forward_propagate(inputs)

    # print results
    print("The network input is {}".format(inputs))
    print("The network output is {}".format(outputs))