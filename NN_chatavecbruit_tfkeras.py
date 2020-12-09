import numpy as np
from random import random
from sklearn.model_selection import train_test_split
import csv
import librosa
import tensorflow.keras as keras

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

## SCRIPT DEEP LEARNING

X = np.array(data["inputs"])
y = np.array(data["outputs"])

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3)

for i in range(len(y)):
    print(y[i].shape)

print(len for i in range(len(data['inputs'])))

# build network topology
model = keras.Sequential([

    # input layer
    # 1st dim: interval
    # 2nd dim: values of Mel freq for this interval
    keras.layers.Flatten(input_shape=(X.shape[1], X.shape[2])),

    # 1st dense layer
    # relu = new activation function
    keras.layers.Dense(512, activation='relu'),

    # 2nd dense layer
    keras.layers.Dense(256, activation='relu'),

    # 3rd dense layer
    keras.layers.Dense(64, activation='relu'),

    # output layer
    # 2 neurones because 2 outputs
    # softmax : converts a vector of numbers into a vector of probabilities,
    # where sum = 1
    keras.layers.Dense(2, activation='softmax')
])

# compile model
optimiser = keras.optimizers.Adam(learning_rate=0.0001)
model.compile(optimizer=optimiser,
                loss='sparse_categorical_crossentropy',
                metrics=['accuracy'])

model.summary()

# train model
history = model.fit(X_train, y_train, epochs=50)

# evaluate model
eval = model.evaluate(X_test, y_test)
