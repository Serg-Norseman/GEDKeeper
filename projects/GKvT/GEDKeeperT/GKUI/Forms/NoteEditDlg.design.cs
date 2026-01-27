using Terminal.Gui.Views;

namespace GKUI.Forms
{
    partial class NoteEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TextView txtNote;

        private void InitializeComponent()
        {
            txtNote = new TextView() { X = 1, Y = 1, Width = 56, Height = 15, Multiline = true, WordWrap = true };
            Add(txtNote);

            btnAccept = new Button() { IsDefault = true };
            btnAccept.Accepted += AcceptClickHandler;
            AddButton(btnAccept);

            btnCancel = new Button() { };
            btnCancel.Accepted += CancelClickHandler;
            AddButton(btnCancel);

            Width = 62;
            Height = 22;
        }
    }
}
