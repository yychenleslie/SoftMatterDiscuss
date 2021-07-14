
class Human():
    
    def __init__(self, name, sex, height, weight):
        self.name = name
        self.sex = sex 
        self.height = height
        self.weight = weight
    
    def get_sex(self):
        return self.sex

    def get_basic_info(self):
        print('Name: {}'.format(self.name))
        print('Sex:{}'.format(self.get_sex()))
        print('BMI:{}'.format(self.BMI()))

    def BMI(self):
        bmi = self.weight/(self.height**2)
    
        if bmi < 20:
            print('Slim')
        elif bmi >=20 and bmi < 25:
            print('Normal')
        else:
            print('Overweight')
        return bmi


xiaomin = Human(name = 'Xiaomin', sex='Male', height=1.5, weight=30)

print('Height:{}'.format(xiaomin.height))
print('Weight:{}'.format(xiaomin.weight))
sex = xiaomin.get_sex()
print('Sex: {}'.format(sex))
xiaomin.get_basic_info()


