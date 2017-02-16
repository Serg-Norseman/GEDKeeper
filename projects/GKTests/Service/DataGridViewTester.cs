using System.Windows.Forms;

namespace NUnit.Extensions.Forms
{
    public class DataGridViewTester : ControlTester<DataGridView, DataGridViewTester>
    {
        public DataGridViewTester()
        {
        }

        public DataGridViewTester(string name, Form form) : base(name, form)
        {
        }

        public new DataGridView Properties
        {
            get { return (DataGridView) TheObject; }
        }

        public void SelectCell(int row, int col)
        {
            Properties.CurrentCell = Properties.Rows[row].Cells[col];
        }
    }
}
