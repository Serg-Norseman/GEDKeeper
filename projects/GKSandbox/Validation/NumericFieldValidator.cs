using System;
using GKCommon;

namespace GKCommon.Validation
{
    public class NumericFieldValidator : ValidatorBase
    {
        internal const string NumericFieldRequire = "Require numeric value";

        public NumericFieldValidator()
        {
            base.ErrorMessage = NumericFieldRequire;
        }

        protected override bool EvaluateIsValid()
        {
            string text = base.ControlToValidate.Text;
            bool result;
            try
            {
                double.Parse(text);
                result = true;
            }
            catch
            {
                result = false;
            }
            return result;
        }
    }
}
