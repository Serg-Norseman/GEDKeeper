#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class RelationshipCalculatorDlg
    {
        private Label lblKinship;
        private TextView txtResult;
        private Button btnRec2Select;
        private Button btnRec1Select;
        private Button btnSwap;
        private TextField Edit2;
        private TextField Edit1;
        private Label Lab2;
        private Label Lab1;
        private Button btnClose;
        
        private void InitializeComponent()
        {
            btnClose = new Button();
            Lab1 = new Label();
            Lab2 = new Label();
            Edit1 = new TextField();
            Edit2 = new TextField();
            btnRec1Select = new Button();
            btnRec2Select = new Button();
            txtResult = new TextView();
            lblKinship = new Label();
            btnSwap = new Button();

            Lab1.Location = new Point(1, 0);
            Lab1.TabIndex = 0;

            Edit1.Location = new Point(1, 1);
            Edit1.ReadOnly = true;
            Edit1.Size = new Size(40, 1);
            Edit1.TabIndex = 1;

            btnRec1Select.Location = new Point(43, 1);
            btnRec1Select.Size = new Size(10, 1);
            btnRec1Select.TabIndex = 2;
            btnRec1Select.Clicked += btnRec1Select_Click;

            Lab2.Location = new Point(1, 3);
            Lab2.TabIndex = 3;

            Edit2.Location = new Point(1, 4);
            Edit2.ReadOnly = true;
            Edit2.Size = new Size(40, 1);
            Edit2.TabIndex = 4;

            btnRec2Select.Location = new Point(43, 4);
            btnRec2Select.Size = new Size(10, 1);
            btnRec2Select.TabIndex = 5;
            btnRec2Select.Clicked += btnRec2Select_Click;

            lblKinship.Location = new Point(1, 6);
            lblKinship.TabIndex = 6;

            txtResult.Location = new Point(1, 7);
            txtResult.Multiline = true;
            txtResult.ReadOnly = true;
            txtResult.Size = new Size(56, 10);
            txtResult.TabIndex = 7;

            btnSwap.Size = new Size(16, 1);
            btnSwap.TabIndex = 8;
            btnSwap.Clicked += btnSwap_Click;

            btnClose.Size = new Size(16, 1);
            btnClose.TabIndex = 9;

            Size = new Size(60, 22);
            Add(txtResult);
            Add(Lab1);
            Add(lblKinship);
            Add(Lab2);
            Add(Edit1);
            Add(Edit2);
            Add(btnRec1Select);
            Add(btnRec2Select);
            AddButton(btnSwap);
            AddButton(btnClose);
        }
    }
}
