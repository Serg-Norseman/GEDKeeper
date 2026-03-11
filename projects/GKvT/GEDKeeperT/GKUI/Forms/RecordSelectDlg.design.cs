#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class RecordSelectDlg
    {
        private Button btnSelect;
        private Button btnCreate;
        private Button btnCancel;
        private FrameView panList;
        public ComboBox txtFastFilter;

        private void InitializeComponent()
        {
            btnSelect = new Button();
            btnCreate = new Button();
            btnCancel = new Button();
            txtFastFilter = new ComboBox();
            panList = new FrameView();

            btnSelect.Size = new Size(16, 1);
            btnSelect.TabIndex = 2;
            btnSelect.Clicked += btnSelect_Click;

            btnCreate.Size = new Size(16, 1);
            btnCreate.TabIndex = 3;
            btnCreate.Clicked += btnCreate_Click;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 4;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnSelect);
            AddButton(btnCreate);
            AddButton(btnCancel);

            txtFastFilter.Location = new Point(1, 1);
            txtFastFilter.Size = new Size(60, 2);
            txtFastFilter.TabIndex = 0;
            txtFastFilter.KeyDown += txtFastFilter_KeyDown;
            //txtFastFilter.TextChanged += txtFastFilter_TextChanged;

            panList.Location = new Point(1, 3);
            panList.Size = new Size(60, 35);
            panList.TabIndex = 1;

            Size = new Size(64, 42);
            Add(txtFastFilter);
            Add(panList);
        }
    }
}
