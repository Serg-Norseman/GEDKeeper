/*
 *  ExpCalculator, simple calculator for standard expressions
 *  Author: Ivlev M.Dmitry (mailto:Dimon@Diogen.nstu.nsk.su)
 *  Patched: Sergey Pedora (mailto:Sergey@mail.fact400.ru)
 *
 *  C# implementation:
 *  Copyright (C) 2011 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 */

using System;
using System.Collections.Generic;

namespace GKCommon
{
    [Serializable]
    public class CalculateException : Exception
    {
        public CalculateException() {}
        public CalculateException(string message) : base(message) {}
    }

    public delegate bool GetVarEventHandler(object sender, string varName, ref double varValue);

    /// <summary>
    /// 
    /// </summary>
    public sealed class ExpCalculator
    {
        #region Private members

        private const double PI = 3.1415926535897931;
        private const double E = 2.718281828;

        private class NamedVar
        {
            public string Name;
            public double Value;
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

        private double fvalue;
        private string svalue;
        private string fExpression;
        private int fPtr;
        private ExpToken fToken;
        private readonly List<NamedVar> fVars;
        private bool fCaseSensitive;

        //private static readonly object EventGetVar;

        #endregion

        #region Instance control

        public bool CaseSensitive
        {
            get { return this.fCaseSensitive; }
            set { this.fCaseSensitive = value; }
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
            this.fVars = new List<NamedVar>();
            this.fCaseSensitive = false;
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
            this.fVars.Clear();
        }

        #endregion

        #region Private methods

        private static void raiseError(string msg)
        {
            throw new CalculateException(msg);
        }

        private static double bool2float(bool B)
        {
            return ((B) ? 1.0 : 0.0);
        }

        private static double fmod(double x, double y)
        {
            return (x - fint((x / y)) * y);
        }

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
                        val = PI;
                    } else if (name == "e") {
                        val = E;
                    } else {
                        val = this.GetVar(name);
                        if (double.IsNaN(val)) {
                            result = this.DoGetVar(name, ref val);
                        }
                    }
                    break;

                case CallbackType.SetValue:
                    this.SetVar(name, val);
                    break;

                case CallbackType.Function:
                    result = DefaultFunction(name, ref val);
                    break;
            }

            if (!result) raiseError("Unknown function or variable \"" + name + "\".");
        }

        private bool DoGetVar(string varName, ref double varValue)
        {
            GetVarEventHandler eventHandler = (GetVarEventHandler)this.OnGetVar; //base.Events[ExpCalculator.EventGetVar];
            return (eventHandler != null) && eventHandler(this, varName, ref varValue);
        }

        private bool ConvertNumber(int first, int last, ushort numBase)
        {
            this.fvalue = 0.0;

            while (first < last)
            {
                char ch = this.fExpression[first];
                byte c = (byte)((int)ch - 48);
                if (c > 9)
                {
                    c -= 7;

                    if (c > 15) {
                        c -= 32;
                    }
                }

                if (c >= numBase) {
                    break;
                }

                this.fvalue = (this.fvalue * numBase + c);
                first++;
            }

            return (first == last);
        }

        private void lex()
        {
            while (this.fExpression[this.fPtr] != '\0' && this.fExpression[this.fPtr] <= ' ')
            {
                this.fPtr++;
            }

            this.fToken = ExpToken.tkEOF;

            if (this.fExpression[this.fPtr] != '\0')
            {
                int s_pos = this.fPtr;
                this.fToken = ExpToken.tkNUMBER;

                char lc;
                if (this.fExpression[this.fPtr] == '$')
                {
                    // hex numbers
                    this.fPtr++;
                    s_pos = this.fPtr;
                    while (true)
                    {
                        lc = this.fExpression[this.fPtr];
                        if (lc < '0' || (lc > '9' && (lc < 'A' || (lc > 'F' && (lc < 'a' || lc > 'f'))))) {
                            break;
                        }
                        this.fPtr++;
                    }
                    if (this.ConvertNumber(s_pos, this.fPtr, 16)) {
                        return;
                    }
                }
                else
                {
                    lc = this.fExpression[this.fPtr];
                    if (lc >= '0' && lc <= '9')
                    {
                        if (lc == '0')
                        {
                            this.fPtr++;
                            lc = this.fExpression[this.fPtr];
                            if (lc == 'X' || lc == 'x')
                            {
                                // hex numbers
                                this.fPtr++;
                                s_pos = this.fPtr;
                                while (true)
                                {
                                    lc = this.fExpression[this.fPtr];
                                    if (lc < '0' || (lc > '9' && (lc < 'A' || (lc > 'F' && (lc < 'a' || lc > 'f'))))) {
                                        break;
                                    }
                                    this.fPtr++;
                                }
                                if (this.ConvertNumber(s_pos, this.fPtr, 16)) {
                                    return;
                                }
                                goto Error;
                            }
                            else
                            {
                                lc = this.fExpression[this.fPtr];
                                if (lc == 'B' || lc == 'b')
                                {
                                    // binary numbers
                                    this.fPtr++;
                                    s_pos = this.fPtr;
                                    while (true)
                                    {
                                        lc = this.fExpression[this.fPtr];
                                        if (lc < '0' || lc > '1') {
                                            break;
                                        }
                                        this.fPtr++;
                                    }
                                    if (this.ConvertNumber(s_pos, this.fPtr, 2)) {
                                        return;
                                    }
                                    goto Error;
                                }
                            }
                        }

                        while (true)
                        {
                            lc = this.fExpression[this.fPtr];
                            if (lc < '0' || (lc > '9' && (lc < 'A' || (lc > 'F' && (lc < 'a' || lc > 'f'))))) {
                                break;
                            }
                            this.fPtr++;
                        }

                        lc = this.fExpression[this.fPtr];
                        if (lc == 'H' || lc == 'h')
                        {
                            if (this.ConvertNumber(s_pos, this.fPtr, 16)) {
                                this.fPtr++;
                                return;
                            }
                        }
                        else
                        {
                            lc = this.fExpression[this.fPtr];
                            if (lc == 'B' || lc == 'b')
                            {
                                if (this.ConvertNumber(s_pos, this.fPtr, 2)) {
                                    this.fPtr++;
                                    return;
                                }
                            }
                            else
                            {
                                if (this.ConvertNumber(s_pos, this.fPtr, 10))
                                {
                                    if (this.fExpression[this.fPtr] == '`')
                                    {
                                        this.fvalue = (this.fvalue * PI / 180.0);
                                        this.fPtr++;
                                        double frac = 0.0;
                                        while (true)
                                        {
                                            lc = this.fExpression[this.fPtr];
                                            if (lc < '0' || lc > '9')
                                            {
                                                break;
                                            }
                                            frac = (frac * 10.0 + ((int)this.fExpression[this.fPtr] - 48));
                                            this.fPtr++;
                                        }
                                        this.fvalue = (this.fvalue + frac * PI / 180.0 / 60.0);
                                        if (this.fExpression[this.fPtr] == '`')
                                        {
                                            this.fPtr++;
                                            frac = 0.0;
                                            while (true)
                                            {
                                                lc = this.fExpression[this.fPtr];
                                                if (lc < '0' || lc > '9')
                                                {
                                                    break;
                                                }
                                                frac = (frac * 10.0 + ((int)this.fExpression[this.fPtr] - 48));
                                                this.fPtr++;
                                            }
                                            this.fvalue = (this.fvalue + frac * PI / 180.0 / 60.0 / 60.0);
                                        }
                                        this.fvalue = fmod(this.fvalue, 6.2831853071795862);
                                        return;
                                    }

                                    if (this.fExpression[this.fPtr] == '.')
                                    {
                                        this.fPtr++;
                                        double frac = 1.0;
                                        while (true)
                                        {
                                            lc = this.fExpression[this.fPtr];
                                            if (lc < '0' || lc > '9')
                                            {
                                                break;
                                            }
                                            frac = (frac / 10.0);
                                            this.fvalue = (this.fvalue + frac * ((int)this.fExpression[this.fPtr] - 48));
                                            this.fPtr++;
                                        }
                                    }

                                    lc = this.fExpression[this.fPtr];
                                    if (lc != 'E' && lc != 'e')
                                    {
                                        return;
                                    }

                                    this.fPtr++;
                                    int exp = 0;
                                    char sign = this.fExpression[this.fPtr];
                                    if (sign == '+' || sign == '-') {
                                        this.fPtr++;
                                    }

                                    lc = this.fExpression[this.fPtr];
                                    if (lc >= '0' && lc <= '9')
                                    {
                                        while (true)
                                        {
                                            lc = this.fExpression[this.fPtr];
                                            if (lc < '0' || lc > '9') {
                                                break;
                                            }
                                            exp = exp * 10 + (int)this.fExpression[this.fPtr] - 48;
                                            this.fPtr++;
                                        }

                                        if (exp == 0) {
                                            return;
                                        }

                                        if (sign == '-')
                                        {
                                            while (exp > 0)
                                            {
                                                this.fvalue = (this.fvalue / 10.0);
                                                exp--;
                                            }
                                            return;
                                        }
                                        else
                                        {
                                            while (exp > 0)
                                            {
                                                this.fvalue = (this.fvalue * 10.0);
                                                exp--;
                                            }
                                            return;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        lc = this.fExpression[this.fPtr];
                        if (lc >= 'A' && (lc < '[' || lc == '_' || (lc >= 'a' && lc < '{')))
                        {
                            this.svalue = new string(this.fExpression[this.fPtr], 1);
                            this.fPtr++;
                            while (true)
                            {
                                lc = this.fExpression[this.fPtr];
                                if (lc < '0' || (lc > '9' && (lc < 'A' || (lc >= '[' && lc != '_' && (lc < 'a' || lc >= '{')))))
                                {
                                    break;
                                }
                                string text = this.svalue;
                                if (((text != null) ? text.Length : 0) >= 32)
                                {
                                    break;
                                }
                                this.svalue += this.fExpression[this.fPtr];
                                this.fPtr++;
                            }
                            this.fToken = ExpToken.tkIDENT;
                            return;
                        }

                        char c = this.fExpression[this.fPtr];
                        this.fPtr++;

                        switch (c)
                        {
                            case '!':
                                this.fToken = ExpToken.tkNOT;
                                if (this.fExpression[this.fPtr] == '=')
                                {
                                    this.fPtr++;
                                    this.fToken = ExpToken.tkNE;
                                    return;
                                }
                                return;

                            case '%':
                                this.fToken = ExpToken.tkMOD;
                                if (this.fExpression[this.fPtr] == '%')
                                {
                                    this.fPtr++;
                                    this.fToken = ExpToken.tkPER;
                                    return;
                                }
                                return;

                            case '&':
                                this.fToken = ExpToken.tkAND;
                                return;

                            case '(':
                                this.fToken = ExpToken.tkLBRACE;
                                return;

                            case ')':
                                this.fToken = ExpToken.tkRBRACE;
                                return;

                            case '*':
                                this.fToken = ExpToken.tkMUL;
                                if (this.fExpression[this.fPtr] == '*')
                                {
                                    this.fPtr++;
                                    this.fToken = ExpToken.tkPOW;
                                    return;
                                }
                                return;

                            case '+':
                                this.fToken = ExpToken.tkADD;
                                return;

                            case '-':
                                this.fToken = ExpToken.tkSUB;
                                return;

                            case '/':
                                this.fToken = ExpToken.tkDIV;
                                return;

                            case ';':
                                this.fToken = ExpToken.tkSEMICOLON;
                                return;

                            case '<':
                                this.fToken = ExpToken.tkLT;
                                if (this.fExpression[this.fPtr] == '=')
                                {
                                    this.fPtr++;
                                    this.fToken = ExpToken.tkLE;
                                    return;
                                }
                                return;

                            case '=':
                                this.fToken = ExpToken.tkASSIGN;
                                if (this.fExpression[this.fPtr] == '=')
                                {
                                    this.fPtr++;
                                    this.fToken = ExpToken.tkEQ;
                                    return;
                                }
                                return;

                            case '>':
                                this.fToken = ExpToken.tkGT;
                                if (this.fExpression[this.fPtr] == '=')
                                {
                                    this.fPtr++;
                                    this.fToken = ExpToken.tkGE;
                                    return;
                                }
                                return;

                            case '^':
                                this.fToken = ExpToken.tkXOR;
                                return;

                            case '|':
                                this.fToken = ExpToken.tkOR;
                                return;

                            case '~':
                                this.fToken = ExpToken.tkINV;
                                return;

                            default:
                                this.fToken = ExpToken.tkERROR;
                                this.fPtr--;
                                return;
                        }
                    }
                }

            Error:
                this.fToken = ExpToken.tkERROR;
            }
        }

        private void checkToken(ExpToken expected)
        {
            if (this.fToken != expected) raiseError("Syntax error");
        }

        private void term(ref double R)
        {
            switch (this.fToken)
            {
                case ExpToken.tkLBRACE:
                    this.lex();
                    this.expr6(ref R);
                    this.checkToken(ExpToken.tkRBRACE);
                    this.lex();
                    break;

                case ExpToken.tkNUMBER:
                    R = this.fvalue;
                    this.lex();
                    break;

                case ExpToken.tkIDENT:
                    {
                        string st = this.svalue;
                        if (!this.fCaseSensitive) {
                            st = st.ToLower();
                        }

                        this.lex();
                        switch (this.fToken)
                        {
                            case ExpToken.tkLBRACE:
                                this.lex();
                                if (st == "if") {
                                    this.exprIf(out R);
                                } else {
                                    this.expr6(ref R);
                                    this.DefaultCallback(CallbackType.Function, st, ref R);
                                }
                                this.checkToken(ExpToken.tkRBRACE);
                                this.lex();
                                break;

                            case ExpToken.tkASSIGN:
                                this.lex();
                                this.expr6(ref R);
                                this.DefaultCallback(CallbackType.SetValue, st, ref R);
                                break;

                            default:
                                this.DefaultCallback(CallbackType.GetValue, st, ref R);
                                break;
                        }
                    }
                    break;

                default:
                    raiseError("Syntax error");
                    break;
            }
        }

        private void exprIf(out double R)
        {
            double resCond = 0.0d, resThen = 0.0d, resElse = 0.0d;

            this.expr6(ref resCond);

            this.checkToken(ExpToken.tkSEMICOLON);
            this.lex();
            this.expr6(ref resThen);

            this.checkToken(ExpToken.tkSEMICOLON);
            this.lex();
            this.expr6(ref resElse);

            R = (resCond == 1.0d) ? resThen : resElse;
        }

        private void expr1(ref double R)
        {
            this.term(ref R);

            if (this.fToken == ExpToken.tkPOW)
            {
                this.lex();
                double V = 0.0;
                this.term(ref V);
                R = Math.Pow(R, V);
            }
        }

        private void expr2(ref double R)
        {
            if (fToken >= ExpToken.tkINV && (fToken < ExpToken.tkMUL || (fToken >= ExpToken.tkADD && fToken < ExpToken.tkLT)))
            {
                ExpToken oldt = this.fToken;
                this.lex();
                this.expr2(ref R);

                switch (oldt) {
                    case ExpToken.tkINV:
                        R = (double)(~trunc(R));
                        break;

                    case ExpToken.tkNOT:
                        R = bool2float(trunc(R) <= 0);
                        break;

                    case ExpToken.tkSUB:
                        R = (-R);
                        break;
                }
            } else {
                this.expr1(ref R);
            }
        }

        private void expr3(ref double R)
        {
            this.expr2(ref R);
            while (true)
            {
                if (this.fToken < ExpToken.tkMUL || this.fToken > ExpToken.tkPER) break;

                ExpToken oldt = this.fToken;
                this.lex();
                double V = 0.0;
                this.expr2(ref V);

                switch (oldt) {
                    case ExpToken.tkMUL:
                        R = (R * V);
                        break;

                    case ExpToken.tkDIV:
                        R = (R / V);
                        break;

                    case ExpToken.tkMOD:
                        R = ((double)(trunc(R) % trunc(V)));
                        break;

                    case ExpToken.tkPER:
                        R = ((R / V) * 100.0);
                        break;
                }
            }
        }

        private void expr4(ref double R)
        {
            this.expr3(ref R);
            while (true)
            {
                if (this.fToken < ExpToken.tkADD || this.fToken > ExpToken.tkSUB) break;

                ExpToken oldt = this.fToken;
                this.lex();
                double V = 0.0;
                this.expr3(ref V);

                switch (oldt) {
                    case ExpToken.tkADD:
                        R = (R + V);
                        break;

                    case ExpToken.tkSUB:
                        R = (R - V);
                        break;
                }
            }
        }

        private void expr5(ref double R)
        {
            this.expr4(ref R);
            while (true)
            {
                if (this.fToken < ExpToken.tkLT || this.fToken > ExpToken.tkGT) break;

                ExpToken oldt = this.fToken;
                this.lex();
                double V = 0.0;
                this.expr4(ref V);

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
        }

        private void expr6(ref double R)
        {
            this.expr5(ref R);
            while (true)
            {
                if (this.fToken < ExpToken.tkOR || this.fToken > ExpToken.tkAND) break;

                ExpToken oldt = this.fToken;
                this.lex();
                double V = 0.0;
                this.expr5(ref V);

                switch (oldt) {
                    case ExpToken.tkOR:
                        R = (double)(trunc(R) | trunc(V));
                        break;

                    case ExpToken.tkXOR:
                        R = (double)(trunc(R) ^ trunc(V));
                        break;

                    case ExpToken.tkAND:
                        R = (double)(trunc(R) & trunc(V));
                        break;
                }
            }
        }

        private void expr7(ref double R)
        {
            this.expr6(ref R);
            while (this.fToken == ExpToken.tkSEMICOLON)
            {
                this.lex();
                this.expr6(ref R);
            }
        }

        #endregion

        #region Public methods

        public double Calc(string expression)
        {
            double result = 0.0;

            this.fExpression = expression + "\0";
            this.fPtr = 0;

            this.lex();
            this.expr7(ref result);
            this.checkToken(ExpToken.tkEOF);

            return result;
        }

        public double GetVar(string name)
        {
            int num = this.fVars.Count;
            for (int i = 0; i < num; i++) {
                NamedVar nVar = this.fVars[i];

                if (string.Compare(nVar.Name, name, !this.fCaseSensitive) == 0)
                {
                    return nVar.Value;
                }
            }
            
            return double.NaN;
        }

        public void SetVar(string name, double value)
        {
            NamedVar nVar = null;

            int num = this.fVars.Count;
            for (int i = 0; i < num; i++)
            {
                NamedVar nv = this.fVars[i];
                if (string.Compare(nv.Name, name, false) == 0) {
                    nVar = nv;
                }
            }

            if (nVar == null)
            {
                nVar = new NamedVar();
                nVar.Name = name;
                this.fVars.Add(nVar);
            }

            nVar.Value = value;
        }

        #endregion
    }
}
