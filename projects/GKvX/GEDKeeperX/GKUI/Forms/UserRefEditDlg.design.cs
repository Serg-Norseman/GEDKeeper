using GKUI.Components;
using Plugin.InputKit.Shared.Controls;
using Xamarin.Forms;

namespace GKUI.Forms
{
    partial class UserRefEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblReference;
        private XFComboBox cmbRef;
        private Label lblRefType;
        private XFComboBox cmbRefType;

        private void InitializeComponent()
        {
            btnAccept = UIHelper.CreateDialogButton("btnAccept", btnAccept_Click);
            btnCancel = UIHelper.CreateDialogButton("btnCancel", CancelClickHandler);

            lblReference = new Label();
            lblReference.Text = "lblReference";

            cmbRef = new XFComboBox();
            //cmbRef.IsEditable = true;
            cmbRef.WidthRequest = 400;

            lblRefType = new Label();
            lblRefType.Text = "lblRefType";

            cmbRefType = new XFComboBox();
            //cmbRefType.IsEditable = true;
            cmbRefType.WidthRequest = 400;

            var panelData = new Grid()
            {
                ColumnSpacing = 10,
                RowSpacing = 10,
                //HeightRequest = 175,
                WidthRequest = 600,
                HorizontalOptions = LayoutOptions.Center,
                VerticalOptions = LayoutOptions.Start
            };
            panelData.Children.Add(lblReference, 0, 0);
            panelData.Children.Add(cmbRef, 1, 0);
            panelData.Children.Add(lblRefType, 0, 1);
            panelData.Children.Add(cmbRefType, 1, 1);

            Content = UIHelper.CreateVStackLayout(new View[] {
                panelData,
                UIHelper.CreateHStackLayout(btnAccept, btnCancel)
            });

            ForceLayout();
            Title = "UserRefEditDlg";
            SetPredefProperties(500, 180);
        }
    }
}
