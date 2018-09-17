from flask import Flask, request, render_template, redirect
# render_template: html파일을 client로 전달
app = Flask(__name__)

from dao import schedulerdao
from dao import sungjuk
from dao import kobis
from dao import IDdao

@app.route("/")
def index():
	return render_template("index.html")
	
@app.route("/second")
def second():
	return render_template("second.html")
	
@app.route("/image_map")
def image_map():
	return render_template("image_map.html")
	
@app.route("/table1")
def table1():
	return render_template("table1.html")	
	
@app.route("/table2")
def table2():
	return render_template("table2.html")	
	
@app.route("/javascript1")
def javascript1():
	return render_template("javascript1.html")	
	
@app.route("/for1")
def for1():
	return render_template("for1.html")		
	
@app.route("/javascript2")
def javascript2():
	return render_template("javascript2.html")		
	
@app.route("/sch")
def sch():
	return render_template("scheduler.html")
	
@app.route("/scheduler",methods=["GET","POST","PUT","DELETE"])
def scheduler():
	if request.method == 'GET':
		start = request.args.get('start')
		end = request.args.get('end')
		return schedulerdao.getScheduler({'start':start, 'end':end})
	if request.method == "POST":
		start = request.form['start']
		end = request.form['end']
		title = request.form['title']
		allDay = request.form['allDay']
		schedule = {'title':title, 'start':start, 'end':end, 'allDay':allDay}
		return schedulerdao.setScheduler(schedule)
	if request.method == 'DELETE':
		id = request.form['id']
		return schedulerdao.delScheduler(id)
	if request.method == 'PUT':
		schedule = request.form
		return schedulerdao.putScheduler(schedule)	
		
@app.route("/sungjuk_call")
def sungjuk_call():
	result = sungjuk.getSungjuk() 	
	# print(result)
	return render_template("sungjuk2.html" , object_list=result)

@app.route("/sungjukAct", methods=["GET","POST","PUT","DELETE"])
def sungjukAct():
    if request.method == 'GET':
        return sungjuk.getSungjuk()

    if request.method == 'POST':
        name = request.form['name1']
        kor  = request.form['kor1']
        mat  = request.form['mat1']
        eng  = request.form['eng1']
        sungdata = {'name' : name, 'kor' : kor, 'mat' : mat, 'eng' : eng}
        return  sungjuk.setSungjuk(sungdata)

    if request.method == 'DELETE':
        name = request.form['id']
        return  sungjuk.delSungjuk(name)

    if request.method == 'PUT':
        sungData = request.form
        return sungjuk.putSungjuk(sungData)

@app.route("/crawling")
def crawling():
	result = kobis.getCinema()
	return render_template("crawling.html", object_list=result)
	
@app.route("/kobisAct", methods=["GET", "POST","PUT","DELETE"])
def kobisAct():
	if request.method == 'GET':
		return kobis.getCinema()
		
@app.route("/hing")
def hing():
	return render_template("hing.html")		
	
@app.route("/icons")
def icons():
	return render_template("icons.html")		
	
	
@app.route("/login")
def login():
    result = kobis.getCinema()   
    return render_template("index.html", object_list=result)

@app.route("/check", methods=["POST"])
def check():
    if request.method == "POST":
        id=request.values.get("email")
        pwd = request.values.get("pwd")
        requestdata = {"email": id, "pwd": pwd}
        res = IDdao.getUsers(requestdata)
        if res == 0:
            return redirect("/")
        else:
            return redirect("/sungjuk_call")
			
@app.route("/registration")
def registration():
	return render_template("registration.html")
	
@app.route("/registerAct", methods=["POST"])
def registerAct():			
	if request.method == "POST":
		id=request.values.get("email")
		pwd = request.values.get("pwd")
		requestdata = {"email": id, "pwd": pwd}
		IDdao.setUsers(requestdata)
		return redirect("/sungjuk_call")
		
if __name__ == '__main__':
	app.run(debug=True)
	
	
	
	
	
	
	
	
	
	
	
	