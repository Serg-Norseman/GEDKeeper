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
        public MaskedTextBoxTester(string name, string formName) : base(name, formName)
        {
        }
        public MaskedTextBoxTester(string name) : base(name)
        {
        }
        public MaskedTextBoxTester(MaskedTextBoxTester tester, int index) : base(tester, index)
        {
        }
        public void Enter(string text)
        {
            this.EnterText(text);
        }
    }
}
