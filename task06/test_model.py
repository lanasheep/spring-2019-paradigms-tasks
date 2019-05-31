#!/usr/bin/env python3
import pytest
import model
import sys
from io import StringIO


def test_nothing():
    assert 2 * 2 == 4


def test_set_get():
    scope = model.Scope()

    scope["zero"] = 0
    scope["one"] = 1
    scope[2] = "two"

    assert scope["zero"] == 0
    assert scope["one"] == 1
    assert scope[2] == "two"


def test_scope():
    a, b = object(), object()
    scope = model.Scope()

    scope['a'] = a
    scope['b'] = b

    assert scope['a'] == a
    assert scope['b'] == b


def test_scope_parent():
    a, b, c = object(), object(), object()
    scope_parent = model.Scope()
    scope = model.Scope(scope_parent)

    scope['a'] = a
    scope_parent['a'] = c
    scope_parent['b'] = b

    assert scope['a'] == a
    assert scope['b'] == b
    assert scope_parent['a'] == c


def test_var_is_nonexistent():
    a, b = object(), object()
    scope_parent = model.Scope()
    scope = model.Scope(scope_parent)

    scope['a'] = a
    scope_parent['b'] = b

    with pytest.raises(KeyError) as message:
        scope['c']
    assert str(message.value) == "'c'"


def test_function_definition():
    scope = model.Scope()

    func = model.Function([], [])
    func_def = model.FunctionDefinition("func", func)

    assert func_def.evaluate(scope) == func
    assert scope["func"] == func


def test_conditional():
    scope = model.Scope()

    result_true = model.Number(4)
    result_false = model.Number(13)

    one = model.Number(1)
    zero = model.Number(0)

    conditional_true = model.Conditional(one, [result_true], [result_false])
    conditional_false = model.Conditional(zero, [result_true], [result_false])

    assert conditional_true.evaluate(scope) == result_true
    assert conditional_false.evaluate(scope) == result_false


def test_print(capsys):
    scope = model.Scope()
    model.Print(model.Number(12345)).evaluate(scope)
    out, err = capsys.readouterr()

    assert out == '12345\n'
    assert err == ''


def test_read(monkeypatch):
    monkeypatch.setattr(sys, 'stdin', StringIO('12345'))
    scope = model.Scope()
    a = model.Number(12345)

    print(12345)

    assert model.Read("var").evaluate(scope) == a
    assert scope["var"] == a


def test_function_call():
    scope = model.Scope()
    func = model.Function(["var"], [model.Reference("var")])
    a = model.Number(100)

    assert model.FunctionCall(func, [a]).evaluate(scope) == a


def test_reference():
    scope = model.Scope()
    number = model.Number(5)
    func = model.Function([], [])

    scope["var"] = number
    scope["func"] = func

    assert model.Reference("var").evaluate(scope) == number
    assert model.Reference("func").evaluate(scope) == func


def test_binary_operation():
    scope = model.Scope()

    a = model.Number(2)
    b = model.Number(3)

    assert model.BinaryOperation(a, "+", b).evaluate(scope).value == 5
    assert model.BinaryOperation(a, "*", b).evaluate(scope).value == 6
    assert model.BinaryOperation(a, "%", b).evaluate(scope).value == 2


def test_unary_operation():
    scope = model.Scope()
    a = model.Number(1)

    assert model.UnaryOperation('-', a).evaluate(scope).value == -1
    assert model.UnaryOperation('!', a).evaluate(scope).value == 0


def test_calc_factorial():
    zero = model.Number(0)
    one = model.Number(1)

    iz_zero = model.BinaryOperation(model.Reference('n'), '==', zero)
    subtract_one = model.BinaryOperation(model.Reference('n'), '-', one)

    calc_factorial = model.FunctionDefinition(
        "fac",
        model.Function
        (
            ['n'],
            [
                model.Conditional
                (
                    iz_zero,
                    [one],
                    [
                        model.BinaryOperation
                        (
                            model.Reference('n'),
                            '*',
                            model.FunctionCall
                            (
                                model.Reference("fac"),
                                [subtract_one]
                            )
                        )
                    ]
                )
            ]
        )
    )

    scope = model.Scope()
    calc_factorial.evaluate(scope)

    call_fac_0 = model.FunctionCall(calc_factorial, [model.Number(0)])
    call_fac_5 = model.FunctionCall(calc_factorial, [model.Number(5)])

    assert call_fac_0.evaluate(scope).value == 1
    assert call_fac_5.evaluate(scope).value == 120


if __name__ == "__main__":
    pytest.main()
