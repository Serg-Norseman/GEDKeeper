/*
 *  ExpCalculator, simple calculator for standard expressions
 *  Author: Ivlev M.Dmitry (mailto:Dimon@Diogen.nstu.nsk.su)
 *  Patched: Sergey Pedora (mailto:Sergey@mail.fact400.ru)
 *
 *  C# implementation:
 *  Copyright (C) 2011 by Sergey V. Zhdanovskih.
 */

using System;
using System.Collections.Generic;

namespace GKCommon
{
    public class CalculateException : Exception
    {
        public CalculateException(string message) : base(message) {}
    }

    public delegate bool GetVarEventHandler(object sender, string varName, ref double varValue);

    /// <summary>
    /// The simple calculator for standard expressions.
    /// </summary>
    public sealed class ExpCalculator
    {
        #region Private members

        private class NamedVar
        {
            public readonly string Name;
            public double Value;

            public NamedVar(string name)
            {
                Name = name;
            }
        }

        private enum CallbackType
        {
            GetValue,
            SetValue,
            Function
        }

        private enum ExpToken
        {
            tkEOF,
            tkERROR,
            tkASSIGN,
            tkLBRACE,
            tkRBRACE,
            tkNUMBER,
            tkIDENT,
            tkSEMICOLON,
            tkPOW,
            tkINV,
            tkNOT,
            tkMUL,
            tkDIV,
            tkMOD,
            tkPER,
            tkADD,
            tkSUB,
            tkLT,
            tkLE,
            tkEQ,
            tkNE,
            tkGE,
            tkGT,
            tkOR,
            tkXOR,
            tkAND
        }

        private double fValue;
        private string fIdent;
        private ExpToken fToken;
        private readonly List<NamedVar> fVars;
        private bool fCaseSensitive;
        private StringTokenizer fTokenizer;

        //private static readonly object EventGetVar;

        #endregion

        #region Instance control

        public bool CaseSensitive
        {
            get { return fCaseSensitive; }
            set { fCaseSensitive = value; }
        }

        public event GetVarEventHandler OnGetVar;/*
		{
			add { base.Events.AddHandler(ExpCalculator.EventGetVar, value); }
			remove { base.Events.RemoveHandler(ExpCalculator.EventGetVar, value); }
		}*/
        
        static ExpCalculator()
        {
            //ExpCalculator.EventGetVar = new object();
        }

        public ExpCalculator()
        {
            fVars = new List<NamedVar>();
            fCaseSensitive = false;
            fTokenizer = null;
        }

        /*protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.ClearVars();
            }
            base.Dispose(disposing);
        }*/

        public void ClearVars()
        {
            fVars.Clear();
        }

        #endregion

        #region Private methods

        private static double bool2float(bool B)
        {
            return ((B) ? 1.0 : 0.0);
        }

        /*private static double fmod(double x, double y)
        {
            return (x - fint((x / y)) * y);
        }*/

        private static long trunc(double value)
        {
            return (long)Math.Truncate(value);
        }

        private static double fint(double value)
        {
            return ((value > (double)0f) ? Math.Floor(value) : Math.Ceiling(value));
        }

        private static double frac(double value)
        {
            return (value - fint(value));
        }

        private static bool DefaultFunction(string name, ref double val)
        {
            bool result = true;

            if (name == "round")
            {
                val = (long)Math.Round(val);
            }
            else if (name == "trunc")
            {
                val = Math.Truncate(val);
            }
            else if (name == "int")
            {
                val = fint(val);
            }
            else if (name == "frac")
            {
                val = frac(val);
            }
            else if (name == "sin")
            {
                val = Math.Sin(val);
            }
            else if (name == "cos")
            {
                val = Math.Cos(val);
            }
            else if (name == "tan")
            {
                val = Math.Tan(val);
            }
            else if (name == "atan")
            {
                val = Math.Atan(val);
            }
            else if (name == "ln")
            {
                val = Math.Log(val);
            }
            else if (name == "exp")
            {
                val = Math.Exp(val);
            }
            else if (name == "sign")
            {
                if (val > 0.0d) {
                    val = 1.0;
                } else if (val < 0.0d) {
                    val = -1.0;
                }
            }
            else {
                result = false;
            }

            return result;
        }

        private void DefaultCallback(CallbackType ctype, string name, ref double val)
        {
            bool result = true;

            switch (ctype) {
                case CallbackType.GetValue:
                    if (name == "pi") {
                        val = Math.PI;
                    } else if (name == "e") {
                        val = Math.E;
                    } else {
                        val = GetVar(name);
                        if (double.IsNaN(val)) {
                            result = DoGetVar(name, ref val);
                        }
                    }
                    break;

                case CallbackType.SetValue:
                    SetVar(name, val);
                    break;

                case CallbackType.Function:
                    result = DefaultFunction(name, ref val);
                    break;
            }

            if (!result)
                throw new CalculateException("Unknown function or variable \"" + name + "\".");
        }

        private bool DoGetVar(string varName, ref double varValue)
        {
            GetVarEventHandler eventHandler = OnGetVar; //base.Events[ExpCalculator.EventGetVar];
            return (eventHandler != null) && eventHandler(this, varName, ref varValue);
        }

        private void lex()
        {
            Token tok = fTokenizer.CurrentToken;

            switch (tok.Kind)
            {
                case TokenKind.Number:
                    fToken = ExpToken.tkNUMBER;
                    fValue = SysUtils.ParseFloat(tok.Value, double.NaN);
                    break;

                case TokenKind.Symbol:
                    {
                        switch (tok.Value[0])
                        {
                            case '!':
                                fToken = ExpToken.tkNOT;
                                tok = fTokenizer.Next();
                                if (tok.Value[0] == '=') {
                                    fToken = ExpToken.tkNE;
                                } else return;
                                break;

                            case '%':
                                fToken = ExpToken.tkMOD;
                                tok = fTokenizer.Next();
                                if (tok.Value[0] == '%') {
                                    fToken = ExpToken.tkPER;
                                } else return;
                                break;

                            case '&':
                                fToken = ExpToken.tkAND;
                                break;

                            case '(':
                                fToken = ExpToken.tkLBRACE;
                                break;

                            case ')':
                                fToken = ExpToken.tkRBRACE;
                                break;

                            case '*':
                                fToken = ExpToken.tkMUL;
                                tok = fTokenizer.Next();
                                if (tok.Value[0] == '*') {
                                    fToken = ExpToken.tkPOW;
                                } else return;
                                break;

                            case '+':
                                fToken = ExpToken.tkADD;
                                break;

                            case '-':
                                fToken = ExpToken.tkSUB;
                                break;

                            case '/':
                                fToken = ExpToken.tkDIV;
                                break;

                            case ';':
                                fToken = ExpToken.tkSEMICOLON;
                                break;

                            case '<':
                                fToken = ExpToken.tkLT;
                                tok = fTokenizer.Next();
                                if (tok.Value[0] == '=') {
                                    fToken = ExpToken.tkLE;
                                } else return;
                                break;

                            case '=':
                                fToken = ExpToken.tkASSIGN;
                                tok = fTokenizer.Next();
                                if (tok.Value[0] == '=') {
                                    fToken = ExpToken.tkEQ;
                                } else return;
                                break;

                            case '>':
                                fToken = ExpToken.tkGT;
                                tok = fTokenizer.Next();
                                if (tok.Value[0] == '=') {
                                    fToken = ExpToken.tkGE;
                                } else return;
                                break;

                            case '^':
                                fToken = ExpToken.tkXOR;
                                break;

                            case '|':
                                fToken = ExpToken.tkOR;
                                break;

                            case '~':
                                fToken = ExpToken.tkINV;
                                break;

                            default:
                                fToken = ExpToken.tkERROR;
                                break;
                        }
                    }
                    break;

                case TokenKind.Word:
                case TokenKind.Ident:
                    fIdent = tok.Value;
                    fToken = ExpToken.tkIDENT;
                    break;

                case TokenKind.HexNumber:
                    try {
                        fToken = ExpToken.tkNUMBER;
                        fValue = Convert.ToInt32(tok.Value, 16);
                    } catch {
                        fToken = ExpToken.tkERROR;
                    }
                    break;

                case TokenKind.BinNumber:
                    try {
                        fToken = ExpToken.tkNUMBER;
                        fValue = Convert.ToInt32(tok.Value.Substring(2), 2);
                    } catch {
                        fToken = ExpToken.tkERROR;
                    }
                    break;

                case TokenKind.EOF:
                    fToken = ExpToken.tkEOF;
                    break;

                default:
                    fToken = ExpToken.tkERROR;
                    break;
            }

            fTokenizer.Next();
        }

        private void checkToken(ExpToken expected)
        {
            if (fToken != expected)
                throw new CalculateException("Syntax error");
        }

        private double term()
        {
            double R = 0.0d;

            switch (fToken)
            {
                case ExpToken.tkLBRACE:
                    lex();
                    R = expr6();
                    checkToken(ExpToken.tkRBRACE);
                    lex();
                    break;

                case ExpToken.tkNUMBER:
                    R = fValue;
                    lex();
                    break;

                case ExpToken.tkIDENT:
                    {
                        string st = fIdent;
                        if (!fCaseSensitive) {
                            st = st.ToLower();
                        }

                        lex();
                        switch (fToken)
                        {
                            case ExpToken.tkLBRACE:
                                lex();
                                if (st == "if") {
                                    exprIf(out R);
                                } else {
                                    R = expr6();
                                    DefaultCallback(CallbackType.Function, st, ref R);
                                }
                                checkToken(ExpToken.tkRBRACE);
                                lex();
                                break;

                            case ExpToken.tkASSIGN:
                                lex();
                                R = expr6();
                                DefaultCallback(CallbackType.SetValue, st, ref R);
                                break;

                            default:
                                DefaultCallback(CallbackType.GetValue, st, ref R);
                                break;
                        }
                    }
                    break;

                default:
                    throw new CalculateException("Syntax error");
            }

            return R;
        }

        private void exprIf(out double R)
        {
            double resCond = 0.0d, resThen = 0.0d, resElse = 0.0d;

            resCond = expr6();

            checkToken(ExpToken.tkSEMICOLON);
            lex();
            resThen = expr6();

            checkToken(ExpToken.tkSEMICOLON);
            lex();
            resElse = expr6();

            R = (resCond == 1.0d) ? resThen : resElse;
        }

        private double expr1()
        {
            double R = term();

            if (fToken == ExpToken.tkPOW)
            {
                lex();
                double V = term();
                R = Math.Pow(R, V);
            }

            return R;
        }

        private double expr2()
        {
            double R;

            if (fToken >= ExpToken.tkINV && (fToken < ExpToken.tkMUL || (fToken >= ExpToken.tkADD && fToken < ExpToken.tkLT)))
            {
                ExpToken oldt = fToken;
                lex();
                R = expr2();

                switch (oldt) {
                    case ExpToken.tkINV:
                        R = ~trunc(R);
                        break;

                    case ExpToken.tkNOT:
                        R = bool2float(trunc(R) <= 0);
                        break;

                    case ExpToken.tkSUB:
                        R = (-R);
                        break;
                }
            } else {
                R = expr1();
            }

            return R;
        }

        private double expr3()
        {
            double R = expr2();

            while (true)
            {
                if (fToken < ExpToken.tkMUL || fToken > ExpToken.tkPER) break;

                ExpToken oldt = fToken;
                lex();
                double V = expr2();

                switch (oldt) {
                    case ExpToken.tkMUL:
                        R = (R * V);
                        break;

                    case ExpToken.tkDIV:
                        R = (R / V);
                        break;

                    case ExpToken.tkMOD:
                        R = trunc(R) % trunc(V);
                        break;

                    case ExpToken.tkPER:
                        R = ((R / V) * 100.0);
                        break;
                }
            }

            return R;
        }

        private double expr4()
        {
            double R = expr3();

            while (true)
            {
                if (fToken < ExpToken.tkADD || fToken > ExpToken.tkSUB) break;

                ExpToken oldt = fToken;
                lex();
                double V = expr3();

                switch (oldt) {
                    case ExpToken.tkADD:
                        R = (R + V);
                        break;

                    case ExpToken.tkSUB:
                        R = (R - V);
                        break;
                }
            }

            return R;
        }

        private double expr5()
        {
            double R = expr4();

            while (true)
            {
                if (fToken < ExpToken.tkLT || fToken > ExpToken.tkGT) break;

                ExpToken oldt = fToken;
                lex();
                double V = expr4();

                switch (oldt) {
                    case ExpToken.tkLT:
                        R = bool2float(R < V);
                        break;

                    case ExpToken.tkLE:
                        R = bool2float(R <= V);
                        break;

                    case ExpToken.tkEQ:
                        R = bool2float(R == V);
                        break;

                    case ExpToken.tkNE:
                        R = bool2float(R != V);
                        break;

                    case ExpToken.tkGE:
                        R = bool2float(R >= V);
                        break;

                    case ExpToken.tkGT:
                        R = bool2float(R > V);
                        break;
                }
            }

            return R;
        }

        private double expr6()
        {
            double R = expr5();

            while (true)
            {
                if (fToken < ExpToken.tkOR || fToken > ExpToken.tkAND) break;

                ExpToken oldt = fToken;
                lex();
                double V = expr5();

                switch (oldt) {
                    case ExpToken.tkOR:
                        R = trunc(R) | trunc(V);
                        break;

                    case ExpToken.tkXOR:
                        R = trunc(R) ^ trunc(V);
                        break;

                    case ExpToken.tkAND:
                        R = trunc(R) & trunc(V);
                        break;
                }
            }

            return R;
        }

        private double expr7()
        {
            double R = expr6();

            while (fToken == ExpToken.tkSEMICOLON)
            {
                lex();
                R = expr6();
            }

            return R;
        }

        #endregion

        #region Public methods

        public double Calc(string expression)
        {
            double result = 0.0;

            fTokenizer = new StringTokenizer(expression);
            fTokenizer.IgnoreWhiteSpace = true;
            fTokenizer.RecognizeDecimals = true;
            fTokenizer.RecognizeHex = true;
            fTokenizer.RecognizeBin = true;
            fTokenizer.Next();

            lex();
            result = expr7();
            checkToken(ExpToken.tkEOF);

            return result;
        }

        public double GetVar(string name)
        {
            int num = fVars.Count;
            for (int i = 0; i < num; i++) {
                NamedVar nVar = fVars[i];

                if (string.Compare(nVar.Name, name, !fCaseSensitive) == 0)
                {
                    return nVar.Value;
                }
            }
            
            return double.NaN;
        }

        public void SetVar(string name, double value)
        {
            NamedVar nVar = null;

            int num = fVars.Count;
            for (int i = 0; i < num; i++)
            {
                NamedVar nv = fVars[i];
                if (string.Compare(nv.Name, name, false) == 0) {
                    nVar = nv;
                }
            }

            if (nVar == null)
            {
                nVar = new NamedVar(name);
                fVars.Add(nVar);
            }

            nVar.Value = value;
        }

        #endregion
    }
}
