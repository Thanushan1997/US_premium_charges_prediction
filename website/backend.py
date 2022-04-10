from flask import Flask,request,render_template,redirect,url_for
import numpy as np 
import joblib

model = joblib.load("GBR.joblib")

# start flask
app = Flask(__name__)

# render default webpage
@app.route('/')
def home():
    return render_template('home.html')

@app.route('/about')
def about():
    return render_template('index.html')

@app.route('/service')
def service():
    return render_template('services.html')

@app.route('/predict', methods=['POST'])
def get_data():
        age = request.form['Age']
        sex = request.form['sex']
        bmi = request.form['bmi']
        noc = request.form['noc']
        smoker = request.form['smoker']
        region = request.form['region']
       	age = int(age)
        bmi = float(bmi)
        noc = int(noc)

        if sex == "Male":
        	sex_male = 1
        else :
        	sex_male =0

        if smoker == "Yes":
        	smoker_yes = 1
        else :
        	smoker_yes =0

        if region == "southeast":
        	region_northwest = 0
        	region_southeast = 1
        	region_southwest = 0
        elif region == "southwest":
        	region_northwest = 0
        	region_southeast = 0
        	region_southwest = 1
        elif region == "northwest":
        	region_northwest = 1
        	region_southeast = 0
        	region_southwest = 0
        else :
        	region_northwest = 0
        	region_southeast = 0
        	region_southwest = 0
        


        arr = np.array([[age,bmi,noc,sex_male,smoker_yes,region_northwest,region_southeast,region_northwest]])
        pred = model.predict(arr)

        return render_template('services.html',data=pred)

if __name__ == '__main__':
    app.run(debug=True,port=5000)

