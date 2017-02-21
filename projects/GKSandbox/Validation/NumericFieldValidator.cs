namespace GKCommon.Validation
{
    public class NumericFieldValidator : ValidatorBase
    {
        internal const string NUMERIC_FIELD_REQUIRE = "Require numeric value";

        public NumericFieldValidator()
        {
            ErrorMessage = NUMERIC_FIELD_REQUIRE;
        }

        protected override bool EvaluateIsValid()
        {
            string text = ControlToValidate.Text;
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
