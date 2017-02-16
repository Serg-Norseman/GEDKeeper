namespace GKCommon.Validation
{
    public class RequiredFieldValidator : ValidatorBase
    {
        internal const string TEXT_FIELD_REQUIRE = "Require text value";

        public RequiredFieldValidator()
        {
            ErrorMessage = TEXT_FIELD_REQUIRE;
        }

        protected override bool EvaluateIsValid()
        {
            string text = ControlToValidate.Text.Trim();
            return !string.IsNullOrEmpty(text);
        }
    }
}
