using System.Windows.Forms;

namespace NUnit.Extensions.Forms
{
    public class NumericUpDownTester : ControlTester<NumericUpDown, NumericUpDownTester>
    {
        public NumericUpDownTester()
        {
        }

        public NumericUpDownTester(string name, Form form) : base(name, form)
        {
        }

        public new NumericUpDown Properties
        {
            get { return (NumericUpDown) base.TheObject; }
        }

        public void EnterValue(decimal value)
        {
            this.Properties.Value = value;
        }
    }
}
