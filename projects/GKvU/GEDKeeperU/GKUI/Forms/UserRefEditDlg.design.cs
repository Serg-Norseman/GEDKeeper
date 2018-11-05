using GKUI.Components;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;

namespace GKUI.Forms
{
    partial class UserRefEditDlg
    {
        private ImageButton btnAccept;
        private ImageButton btnCancel;
        private TextBlock lblReference;
        private EditableComboBox cmbRef;
        private TextBlock lblRefType;
        private EditableComboBox cmbRefType;

        private void InitializeComponentEx()
        {
            btnAccept = UIHelper.CreateDialogButton("btnAccept", btnAccept_Click);
            btnCancel = UIHelper.CreateDialogButton("btnCancel", CancelClickHandler);

            lblReference = new TextBlock();
            lblReference.Text = "lblReference";

            cmbRef = new EditableComboBox();
            cmbRef.MinWidth = 200;

            lblRefType = new TextBlock();
            lblRefType.Text = "lblRefType";

            cmbRefType = new EditableComboBox();
            cmbRefType.MinWidth = 200;

            var panelData = new Grid()
            {
                ColumnSpacing = 10,
                RowSpacing = 10,
            };
            panelData.ColumnDefinitions.Add(new ColumnDefinition() { Width = GridLength.Auto });
            panelData.ColumnDefinitions.Add(new ColumnDefinition() { Width = GridLength.Auto });
            panelData.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });
            panelData.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });
            panelData.SetGridElement(lblReference, 0, 0);
            panelData.SetGridElement(cmbRef, 1, 0);
            panelData.SetGridElement(lblRefType, 0, 1);
            panelData.SetGridElement(cmbRefType, 1, 1);

            Content = UIHelper.CreateVStackLayout(new UIElement[] {
                panelData,
                UIHelper.CreateHStackLayout(btnAccept, btnCancel)
            });

            SetPredefProperties(500, 180);
        }
    }
}
