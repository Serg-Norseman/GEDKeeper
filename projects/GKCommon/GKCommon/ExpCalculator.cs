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
    [Serializable]
    public class CalculateException : Exception
    {
        public CalculateException(string message) : base(message) {}
    }

    public delegate bool GetVarEventHandler(object sender, string varName, ref double varValue);

    /// <summary>
    /// 
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

        private bool ConvertNumber(int first, int last, ushort numBase)
        {
            fValue = 0.0;

            while (first < last)
            {
                char ch = fExpression[first];
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

                fValue = (fValue * numBase + c);
                first++;
            }

            return (first == last);
        }

        private void lex()
        {
            while (fExpression[fPtr] != '\0' && fExpression[fPtr] <= ' ')
            {
                fPtr++;
            }

            if (fExpression[fPtr] == '\0')
            {
                fToken = ExpToken.tkEOF;
                return;
            }

            int s_pos = fPtr;
            fToken = ExpToken.tkNUMBER;

            char lc;
            if (fExpression[fPtr] == '$')
            {
                // hex numbers
                fPtr++;
                s_pos = fPtr;
                while (true)
                {
                    lc = fExpression[fPtr];
                    if (lc < '0' || (lc > '9' && (lc < 'A' || (lc > 'F' && (lc < 'a' || lc > 'f'))))) {
                        break;
                    }
                    fPtr++;
                }
                if (ConvertNumber(s_pos, fPtr, 16)) {
                    return;
                }
            }
            else
            {
                lc = fExpression[fPtr];
                if (lc >= '0' && lc <= '9')
                {
                    if (lc == '0')
                    {
                        fPtr++;
                        lc = fExpression[fPtr];
                        if (lc == 'X' || lc == 'x')
                        {
                            // hex numbers
                            fPtr++;
                            s_pos = fPtr;
                            while (true)
                            {
                                lc = fExpression[fPtr];
                                if (lc < '0' || (lc > '9' && (lc < 'A' || (lc > 'F' && (lc < 'a' || lc > 'f'))))) {
                                    break;
                                }
                                fPtr++;
                            }
                            if (ConvertNumber(s_pos, fPtr, 16)) {
                                return;
                            }
                            goto Error;
                        }
                        else
                        {
                            lc = fExpression[fPtr];
                            if (lc == 'B' || lc == 'b')
                            {
                                // binary numbers
                                fPtr++;
                                s_pos = fPtr;
                                while (true)
                                {
                                    lc = fExpression[fPtr];
                                    if (lc < '0' || lc > '1') {
                                        break;
                                    }
                                    fPtr++;
                                }
                                if (ConvertNumber(s_pos, fPtr, 2)) {
                                    return;
                                }
                                goto Error;
                            }
                        }
                    }

                    while (true)
                    {
                        lc = fExpression[fPtr];
                        if (lc < '0' || (lc > '9' && (lc < 'A' || (lc > 'F' && (lc < 'a' || lc > 'f'))))) {
                            break;
                        }
                        fPtr++;
                    }

                    lc = fExpression[fPtr];
                    if (lc == 'H' || lc == 'h')
                    {
                        if (ConvertNumber(s_pos, fPtr, 16)) {
                            fPtr++;
                            return;
                        }
                    }
                    else
                    {
                        lc = fExpression[fPtr];
                        if (lc == 'B' || lc == 'b')
                        {
                            if (ConvertNumber(s_pos, fPtr, 2)) {
                                fPtr++;
                                return;
                            }
                        }
                        else
                        {
                            if (ConvertNumber(s_pos, fPtr, 10))
                            {
                                if (fExpression[fPtr] == '`')
                                {
                                    fValue = (fValue * Math.PI / 180.0);
                                    fPtr++;
                                    double frac = 0.0;
                                    while (true)
                                    {
                                        lc = fExpression[fPtr];
                                        if (lc < '0' || lc > '9')
                                        {
                                            break;
                                        }
                                        frac = (frac * 10.0 + ((int)fExpression[fPtr] - 48));
                                        fPtr++;
                                    }
                                    fValue = (fValue + frac * Math.PI / 180.0 / 60.0);
                                    if (fExpression[fPtr] == '`')
                                    {
                                        fPtr++;
                                        frac = 0.0;
                                        while (true)
                                        {
                                            lc = fExpression[fPtr];
                                            if (lc < '0' || lc > '9')
                                            {
                                                break;
                                            }
                                            frac = (frac * 10.0 + ((int)fExpression[fPtr] - 48));
                                            fPtr++;
                                        }
                                        fValue = (fValue + frac * Math.PI / 180.0 / 60.0 / 60.0);
                                    }
                                    fValue = fmod(fValue, 6.2831853071795862);
                                    return;
                                }

                                if (fExpression[fPtr] == '.')
                                {
                                    fPtr++;
                                    double frac = 1.0;
                                    while (true)
                                    {
                                        lc = fExpression[fPtr];
                                        if (lc < '0' || lc > '9')
                                        {
                                            break;
                                        }
                                        frac = (frac / 10.0);
                                        fValue = (fValue + frac * ((int)fExpression[fPtr] - 48));
                                        fPtr++;
                                    }
                                }

                                lc = fExpression[fPtr];
                                if (lc != 'E' && lc != 'e')
                                {
                                    return;
                                }

                                fPtr++;
                                int exp = 0;
                                char sign = fExpression[fPtr];
                                if (sign == '+' || sign == '-') {
                                    fPtr++;
                                }

                                lc = fExpression[fPtr];
                                if (lc >= '0' && lc <= '9')
                                {
                                    while (true)
                                    {
                                        lc = fExpression[fPtr];
                                        if (lc < '0' || lc > '9') {
                                            break;
                                        }
                                        exp = exp * 10 + (int)fExpression[fPtr] - 48;
                                        fPtr++;
                                    }

                                    if (exp == 0) {
                                        return;
                                    }

                                    if (sign == '-')
                                    {
                                        while (exp > 0)
                                        {
                                            fValue = (fValue / 10.0);
                                            exp--;
                                        }
                                        return;
                                    }
                                    else
                                    {
                                        while (exp > 0)
                                        {
                                            fValue = (fValue * 10.0);
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
                    lc = fExpression[fPtr];
                    if (lc >= 'A' && (lc < '[' || lc == '_' || (lc >= 'a' && lc < '{')))
                    {
                        fIdent = new string(fExpression[fPtr], 1);
                        fPtr++;
                        while (true)
                        {
                            lc = fExpression[fPtr];
                            if (lc < '0' || (lc > '9' && (lc < 'A' || (lc >= '[' && lc != '_' && (lc < 'a' || lc > 'z')))))
                            {
                                break;
                            }
                            string text = fIdent;
                            if (((text != null) ? text.Length : 0) >= 32)
                            {
                                break;
                            }
                            fIdent += fExpression[fPtr];
                            fPtr++;
                        }
                        fToken = ExpToken.tkIDENT;
                        return;
                    }

                    char c = fExpression[fPtr];
                    fPtr++;

                    switch (c)
                    {
                        case '!':
                            fToken = ExpToken.tkNOT;
                            if (fExpression[fPtr] == '=')
                            {
                                fPtr++;
                                fToken = ExpToken.tkNE;
                                return;
                            }
                            return;

                        case '%':
                            fToken = ExpToken.tkMOD;
                            if (fExpression[fPtr] == '%')
                            {
                                fPtr++;
                                fToken = ExpToken.tkPER;
                                return;
                            }
                            return;

                        case '&':
                            fToken = ExpToken.tkAND;
                            return;

                        case '(':
                            fToken = ExpToken.tkLBRACE;
                            return;

                        case ')':
                            fToken = ExpToken.tkRBRACE;
                            return;

                        case '*':
                            fToken = ExpToken.tkMUL;
                            if (fExpression[fPtr] == '*')
                            {
                                fPtr++;
                                fToken = ExpToken.tkPOW;
                                return;
                            }
                            return;

                        case '+':
                            fToken = ExpToken.tkADD;
                            return;

                        case '-':
                            fToken = ExpToken.tkSUB;
                            return;

                        case '/':
                            fToken = ExpToken.tkDIV;
                            return;

                        case ';':
                            fToken = ExpToken.tkSEMICOLON;
                            return;

                        case '<':
                            fToken = ExpToken.tkLT;
                            if (fExpression[fPtr] == '=')
                            {
                                fPtr++;
                                fToken = ExpToken.tkLE;
                                return;
                            }
                            return;

                        case '=':
                            fToken = ExpToken.tkASSIGN;
                            if (fExpression[fPtr] == '=')
                            {
                                fPtr++;
                                fToken = ExpToken.tkEQ;
                                return;
                            }
                            return;

                        case '>':
                            fToken = ExpToken.tkGT;
                            if (fExpression[fPtr] == '=')
                            {
                                fPtr++;
                                fToken = ExpToken.tkGE;
                                return;
                            }
                            return;

                        case '^':
                            fToken = ExpToken.tkXOR;
                            return;

                        case '|':
                            fToken = ExpToken.tkOR;
                            return;

                        case '~':
                            fToken = ExpToken.tkINV;
                            return;

                        default:
                            fToken = ExpToken.tkERROR;
                            fPtr--;
                            return;
                    }
                }
            }

        Error:
            fToken = ExpToken.tkERROR;
        }

        private void checkToken(ExpToken expected)
        {
            if (fToken != expected)
                throw new CalculateException("Syntax error");
        }

        private void term(ref double R)
        {
            switch (fToken)
            {
                case ExpToken.tkLBRACE:
                    lex();
                    expr6(ref R);
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
                                    expr6(ref R);
                                    DefaultCallback(CallbackType.Function, st, ref R);
                                }
                                checkToken(ExpToken.tkRBRACE);
                                lex();
                                break;

                            case ExpToken.tkASSIGN:
                                lex();
                                expr6(ref R);
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
                    break;
            }
        }

        private void exprIf(out double R)
        {
            double resCond = 0.0d, resThen = 0.0d, resElse = 0.0d;

            expr6(ref resCond);

            checkToken(ExpToken.tkSEMICOLON);
            lex();
            expr6(ref resThen);

            checkToken(ExpToken.tkSEMICOLON);
            lex();
            expr6(ref resElse);

            R = (resCond == 1.0d) ? resThen : resElse;
        }

        private void expr1(ref double R)
        {
            term(ref R);

            if (fToken == ExpToken.tkPOW)
            {
                lex();
                double V = 0.0;
                term(ref V);
                R = Math.Pow(R, V);
            }
        }

        private void expr2(ref double R)
        {
            if (fToken >= ExpToken.tkINV && (fToken < ExpToken.tkMUL || (fToken >= ExpToken.tkADD && fToken < ExpToken.tkLT)))
            {
                ExpToken oldt = fToken;
                lex();
                expr2(ref R);

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
                expr1(ref R);
            }
        }

        private void expr3(ref double R)
        {
            expr2(ref R);
            while (true)
            {
                if (fToken < ExpToken.tkMUL || fToken > ExpToken.tkPER) break;

                ExpToken oldt = fToken;
                lex();
                double V = 0.0;
                expr2(ref V);

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
        }

        private void expr4(ref double R)
        {
            expr3(ref R);
            while (true)
            {
                if (fToken < ExpToken.tkADD || fToken > ExpToken.tkSUB) break;

                ExpToken oldt = fToken;
                lex();
                double V = 0.0;
                expr3(ref V);

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
            expr4(ref R);
            while (true)
            {
                if (fToken < ExpToken.tkLT || fToken > ExpToken.tkGT) break;

                ExpToken oldt = fToken;
                lex();
                double V = 0.0;
                expr4(ref V);

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
            expr5(ref R);
            while (true)
            {
                if (fToken < ExpToken.tkOR || fToken > ExpToken.tkAND) break;

                ExpToken oldt = fToken;
                lex();
                double V = 0.0;
                expr5(ref V);

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
        }

        private void expr7(ref double R)
        {
            expr6(ref R);
            while (fToken == ExpToken.tkSEMICOLON)
            {
                lex();
                expr6(ref R);
            }
        }

        #endregion

        #region Public methods

        public double Calc(string expression)
        {
            double result = 0.0;

            fExpression = expression + "\0";
            fPtr = 0;

            lex();
            expr7(ref result);
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
