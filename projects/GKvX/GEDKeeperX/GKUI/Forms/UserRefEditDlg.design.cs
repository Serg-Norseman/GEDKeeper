using GKUI.Components;
using Xamarin.Forms;

namespace GKUI.Forms
{
    partial class UserRefEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblReference;
        private Picker cmbRef;
        private Label lblRefType;
        private Picker cmbRefType;

        private void InitializeComponent()
        {
            btnAccept = UIHelper.CreateDialogButton("btnAccept", btnAccept_Click);
            btnCancel = UIHelper.CreateDialogButton("btnCancel", CancelClickHandler);

            lblReference = new Label();
            lblReference.Text = "lblReference";

            cmbRef = new Picker();

            lblRefType = new Label();
            lblRefType.Text = "lblRefType";

            cmbRefType = new Picker();

            var panelData = new Grid()
            {
                ColumnSpacing = 10,
                RowSpacing = 10,
                //HeightRequest = 175,
                WidthRequest = 300,
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

            Title = "UserRefEditDlg";
            SetPredefProperties(500, 180);
        }
    }
}
