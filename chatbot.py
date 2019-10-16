import json
import nltk
import numpy as np
import tensorflow as tf
import random
import pandas as pd
import pickle
import os

from nltk.stem.lancaster import LancasterStemmer

cwd=os.path.dirname(os.getcwd())+'/chatbot/'

class ChatBot():
    def __init__(self, **kwargs):
        self.stemmer = LancasterStemmer()
        self.intent = self.load_intent(cwd+'data/intents.json')
        self.training_data = pickle.load(open(cwd+"model/data_pinai", "rb" ))
        self.model = self.load_model()
        self.context = {}
        self.error_threshold = .25
        
        return super().__init__(**kwargs)

    def load_intent(self, dir):
        with open(dir) as json_data:
            return(json.load(json_data))

    def load_model(self):
        global graph
        graph = tf.get_default_graph()

        with open(cwd+'model/model_pinai', 'rb') as f:
            model = pickle.load(f)

        return model

    def clean_up_sentence(self, sentence):
        sentence_words = nltk.word_tokenize(sentence)
        sentence_words = [self.stemmer.stem(word.lower()) for word in sentence_words]

        return sentence_words

    def bow(self, sentence, words, show_details=False):
        sentence_words = self.clean_up_sentence(sentence)
        bag = [0]*len(self.training_data['words'])  
        for s in sentence_words:
            for i,w in enumerate(self.training_data['words']):
                if w == s: 
                    bag[i] = 1
                    if show_details:
                        print ("found in bag: %s" % w)

        return(np.array(bag))

    def classify(self, sentence):
        with graph.as_default():
            input_data = pd.DataFrame([self.bow(sentence, self.training_data['words'])], dtype=float, index=['input'])
            results = self.model.predict([input_data])[0]
            #results =  self.model.predict([self.bow(sentence, self.training_data['words'])])[0]
            results = [[i,r] for i,r in enumerate(results) if r>self.error_threshold]
            results.sort(key=lambda x: x[1], reverse=True)
            return_list = []
            for r in results:
                return_list.append((self.training_data['classes'][r[0]], r[1]))

            return return_list

    def response(self, sentence, userID='123', show_details=False):
        results = self.classify(sentence)

        if results:
            while results:
                for i in self.intent['intents']:
                    if i['tag'] == results[0][0]:
                        if 'context_set' in i:
                            if show_details: print ('context:', i['context_set'])
                            self.context[userID] = i['context_set']

                        if not 'context_filter' in i or \
                            (userID in self.context and 'context_filter' in i and i['context_filter'] == self.context[userID]):
                            #if show_details: print ('tag:', i['tag'])

                            return [i['tag'], random.choice(i['responses'])]

                results.pop(0)
