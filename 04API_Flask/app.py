from flask import Flask, request
import numpy as np
from pickle import load

app = Flask(__name__)
model = load(open('model.pkl', 'rb'))

@app.route('/', methods=['GET'])
def home():
    return "<h1>Está funcionando essa desgraça</h1> <p> Resposta: Sim </p>"

@app.route("/results", methods = ["POST"])

def predict():
    
    r = request.form

    data = [float(x) for x in r.values()]
    data = np.array(data).reshape(1, -1)

    prediction = model.predict(data)
    prediction = str(prediction)
    
    return {'prediction': prediction}

if __name__ == "__main__":
    app.run(debug = True)