using System.Windows.Forms;

namespace NUnit.Extensions.Forms
{
    public class MaskedTextBoxTester : ControlTester<MaskedTextBox, MaskedTextBoxTester>
    {
        public MaskedTextBoxTester()
        {
        }

        public MaskedTextBoxTester(string name, Form form) : base(name, form)
        {
        }

        public void Enter(string text)
        {
            this.EnterText(text);
        }
    }
}
