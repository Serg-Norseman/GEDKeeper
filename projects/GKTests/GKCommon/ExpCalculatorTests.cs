/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GKCommon;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class ExpCalculatorTests
    {
        private static bool GetVarEventHandler(object sender, VarRequestEventArgs eventArgs)
        {
            if (eventArgs.VarName.Equals("alpha")) {
                eventArgs.VarValue = 15.0;
                return true;
            }

            return false;
        }

        [Test]
        public void Test_Common()
        {
            ExpCalculator calc = new ExpCalculator();
            Assert.IsNotNull(calc);

            calc.CaseSensitive = false;
            Assert.AreEqual(false, calc.CaseSensitive);

            calc.OnGetVar += GetVarEventHandler;

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("12+"); }); // syntax error
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("(12+"); }); // syntax error
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("5 + 0x"); }); // syntax error
            Assert.Throws(typeof(CalculateException), () => { calc.Calc(")"); }); // syntax error

            double val = calc.Calc("2 + 7.703 - 3");
            Assert.AreEqual(6.703, Math.Round(val, 3));

            val = calc.Calc("2**3");
            Assert.AreEqual(8.0, val);

            val = calc.Calc("2 * 3");
            Assert.AreEqual(6.0, val);

            val = calc.Calc("3 / 2");
            Assert.AreEqual(1.5, val);

            val = calc.Calc("3 % 2");
            Assert.AreEqual(1.0, val);

            val = calc.Calc("3 %% 2");
            Assert.AreEqual(150.0, val);

            Assert.AreEqual(-2.0005, calc.Calc("-2.0005"));

            Assert.AreEqual(-2.0e+1, calc.Calc("-2.0e+1"));
            Assert.AreEqual(-2.0e-1, calc.Calc("-2.0e-1"));
            Assert.AreEqual(-2.0e0, calc.Calc("-2.0e0"));
            Assert.AreEqual(525.0d, calc.Calc("5.25e+2"));

            // variables
            calc.ClearVars();

            calc.SetVar("a", 10);
            Assert.AreEqual(10, calc.GetVar("a"));
            calc.SetVar("b", 2);
            Assert.AreEqual(2, calc.GetVar("b"));
            calc.SetVar("c", 0.75);
            Assert.AreEqual(0.75, calc.GetVar("c"));

            val = calc.Calc("a+b+c");
            Assert.AreEqual(12.75, val);

            val = calc.Calc("15 / ((a+b)-c)");
            Assert.AreEqual(1.333, Math.Round(val, 3));

            calc.SetVar("a", 20);
            Assert.AreEqual(20, calc.GetVar("a"));

            val = calc.Calc("a+b+c");
            Assert.AreEqual(22.75, val);

            val = calc.Calc("d=a+b+c");
            Assert.AreEqual(22.75, calc.GetVar("d"));

            val = calc.Calc("d = a + b + c; e = d * 2");
            Assert.AreEqual(22.75, calc.GetVar("d"));
            Assert.AreEqual(45.5, calc.GetVar("e"));

            // functions
            val = calc.Calc("round(12.378)");
            Assert.AreEqual(12.0, val);

            val = calc.Calc("round(12.578)");
            Assert.AreEqual(13.0, val);

            val = calc.Calc("trunc(12.578)");
            Assert.AreEqual(12.0, val);

            val = calc.Calc("int(12.578)");
            Assert.AreEqual(12.0, val);

            val = calc.Calc("frac(12.578)");
            Assert.AreEqual(0.578, Math.Round(val, 3));

            val = calc.Calc("sin(2)");
            Assert.AreEqual(0.909, Math.Round(val, 3));

            val = calc.Calc("cos(2)");
            Assert.AreEqual(-0.416, Math.Round(val, 3));

            val = calc.Calc("tan(2)");
            Assert.AreEqual(-2.185, Math.Round(val, 3));

            val = calc.Calc("atan(2)");
            Assert.AreEqual(1.107, Math.Round(val, 3));

            val = calc.Calc("exp(5)");
            Assert.AreEqual(148.413, Math.Round(val, 3));

            val = calc.Calc("ln(117)");
            Assert.AreEqual(4.762, Math.Round(val, 3));

            val = calc.Calc("sign(-15)");
            Assert.AreEqual(-1, Math.Round(val, 3));

            val = calc.Calc("sign(2)");
            Assert.AreEqual(+1, Math.Round(val, 3));

            val = calc.Calc("pi * 2");
            Assert.AreEqual(6.283, Math.Round(val, 3));

            val = calc.Calc("e");
            Assert.AreEqual(2.718, Math.Round(val, 3));

            // logic
            val = calc.Calc("2 < 3");
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("2 > 3");
            Assert.AreEqual(0, Math.Round(val, 0));
            val = calc.Calc("3 <= 3");
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("2 >= 3");
            Assert.AreEqual(0, Math.Round(val, 0));
            val = calc.Calc("3 == 3");
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("2 != 3");
            Assert.AreEqual(1, Math.Round(val, 0));

            // misc
            val = calc.Calc("2 ^ 3"); // xor
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("5 | 2"); // or
            Assert.AreEqual(7, Math.Round(val, 0));
            val = calc.Calc("9 & 5"); // and
            Assert.AreEqual(1, Math.Round(val, 0));
            val = calc.Calc("~15"); // inv
            Assert.AreEqual(-16, Math.Round(val, 0));
            val = calc.Calc("!-15"); // not
            Assert.AreEqual(1, Math.Round(val, 0));

            // vars
            val = calc.Calc("15 - alpha");
            Assert.AreEqual(0, Math.Round(val, 0));

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("15 - beta"); });
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("15\\"); });
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("15\"srfgsdf\""); });
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("abr(15)"); });

            calc.OnGetVar -= GetVarEventHandler;

            // numbers
            val = calc.Calc("1537");
            Assert.AreEqual(1537, Math.Round(val, 0));
            val = calc.Calc("0b11000000001");
            Assert.AreEqual(1537, Math.Round(val, 0));
            val = calc.Calc("0x601");
            Assert.AreEqual(1537, Math.Round(val, 0));

            //val = calc.Calc("1`"); // 1` = 0,01745 rad
            //Assert.AreEqual(0.01745, Math.Round(val, 5));

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("0x15j"); });
            Assert.Throws(typeof(CalculateException), () => { calc.Calc("0b015"); });

            val = calc.Calc("if(3 == 3; 2; 3)");
            Assert.AreEqual(2, Math.Round(val, 0));

            val = calc.Calc("if(2 == 3; 2; 3)");
            Assert.AreEqual(3, Math.Round(val, 0));

            Assert.Throws(typeof(CalculateException), () => { calc.Calc("if(2 == 3)"); }); // syntax error
        }
    }
}
