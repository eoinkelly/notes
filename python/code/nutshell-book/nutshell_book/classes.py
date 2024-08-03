class MyBaseClassA:
    pass


class MyBaseClassB:
    pass


class MyClass(MyBaseClassA, MyBaseClassB):
    pass


#
# class SomeClass(metaClass=MyClass):
# 	pass


# compiles just fine but not encouraged
class mynoInitialCapClass:
    pass


# this ...
class TypeExample:
    x: int = 42


# ... is sugar for ....
TypeExampleNoSugar = type("TypeExampleNoSugar", (), {"x": 42})

my_c = TypeExample()
print(my_c.x)  # 42
my_object = TypeExampleNoSugar()
print(my_object.x)  # 42
