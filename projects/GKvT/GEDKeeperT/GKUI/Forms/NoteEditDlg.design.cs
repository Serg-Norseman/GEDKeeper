using Terminal.Gui;

namespace GKUI.Forms
{
    partial class NoteEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TextView txtNote;

        private void InitializeComponent()
        {
            txtNote = new TextView() { X = 1, Y = 1, Width = 56, Height = 15, Multiline = true, WordWrap = true, TabIndex = 0 };
            Add(txtNote);

            btnAccept = new Button() { TabIndex = 1, IsDefault = true };
            btnAccept.MouseClick += AcceptClickHandler;
            AddButton(btnAccept);

            btnCancel = new Button() { TabIndex = 2 };
            btnCancel.MouseClick += CancelClickHandler;
            AddButton(btnCancel);

            Width = 60;
            Height = 20;
        }
    }
}
