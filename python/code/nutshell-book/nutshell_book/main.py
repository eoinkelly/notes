"""
If the first statement in a file is a string it becomes __doc__ in the module.
"""

# import animal.dog.poodle
from animal.dog import poodle

# import bar
from . import \
    bar  # import bar module from current package, doesn't work at top level but works within packages


def test_indent_string():
    x = """This is a test
of what whitespace is
preserved."""
    print(x)
    return x


if __name__ == "__main__":
    print(poodle.species)
    print(bar.bar_flag)
    # print(bar.foo.foo_flag)
    # print(bar.foo._private_foo_flag)
