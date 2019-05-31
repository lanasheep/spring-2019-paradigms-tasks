#!/usr/bin/env python3
import pytest
from folder import *


def test_nothing():
    assert 2 * 2 == 4


def test_bin_num_num():
    program = fold_constants(BinaryOperation(Number(5), "%", Number(3)))
    program_correct = Number(2)

    assert program == program_correct


def test_bin_num_ref():
    program = fold_constants(BinaryOperation(Number(0), "*", Reference("x")))
    program_correct = Number(0)

    assert program == program_correct


def test_bin_ref_num():
    program = fold_constants(BinaryOperation(Reference("x"), "*", Number(0)))
    program_correct = Number(0)

    assert program == program_correct


def test_bin_ref_ref():
    program = fold_constants(BinaryOperation(Reference("a"), "-",
                                             Reference("a")))
    program_correct = Number(0)

    assert program == program_correct


def test_un_num():
    program = fold_constants(UnaryOperation("-", Number(300)))
    program_correct = Number(-300)

    assert program == program_correct


def test_folder():
    assert fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    ) == Number(13)


if __name__ == "__main__":
    pytest.main()
