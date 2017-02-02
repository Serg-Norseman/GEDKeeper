using System;

namespace GKCommon.Validation
{
    public class RequiredFieldValidator : ValidatorBase
    {
        internal const string TextFieldRequire = "Require text value";

        public RequiredFieldValidator()
        {
            base.ErrorMessage = TextFieldRequire;
        }

        protected override bool EvaluateIsValid()
        {
            string text = base.ControlToValidate.Text.Trim();
            return text != null && !(text == string.Empty);
        }
    }
}
