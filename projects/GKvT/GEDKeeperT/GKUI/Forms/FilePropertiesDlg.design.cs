#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class FilePropertiesDlg
    {
        private Button btnAccept, btnCancel;
        private TabView PageControl1;
        private TabPage pageAuthor, pageOther;
        private Label lblName, lblAddress, lblTelephone, lblLanguage;
        private TextField txtName, txtTel;
        private TextView txtAddress;
        private ComboBox txtLanguage;
        private GKListView lvRecordStats;

        private void InitializeComponent()
        {
            btnAccept = new Button() { Width = 16, TabIndex = 1 };
            btnAccept.Clicked += AcceptClickHandler;
            AddButton(btnAccept);

            btnCancel = new Button() { Width = 16, TabIndex = 2 };
            btnCancel.Clicked += CancelClickHandler;
            AddButton(btnCancel);

            lblName = new Label() { TabIndex = 0 };
            txtName = new TextField() { TabIndex = 1, Width = 54 };
            lblTelephone = new Label() { TabIndex = 2 };
            txtTel = new TextField() { TabIndex = 3, Width = 54 };
            lblAddress = new Label() { TabIndex = 4 };
            txtAddress = new TextView() { TabIndex = 5, Width = 54, Height = 5, Multiline = true, AllowsReturn = true, AllowsTab = false };
            lblLanguage = new Label() { TabIndex = 6 };
            txtLanguage = new ComboBox() { TabIndex = 7, Width = 54, Height = 2 };

            pageAuthor = new TabPage() { View = new StackLayout(Orientation.Vertical, 1, 0, new View[] {
                lblName, txtName, lblTelephone, txtTel, lblAddress, txtAddress, lblLanguage, txtLanguage
            }) };

            lvRecordStats = new GKListView();
            lvRecordStats.Width = Dim.Fill();
            lvRecordStats.Height = Dim.Fill();
            pageOther = new TabPage() { View = lvRecordStats };

            PageControl1 = new TabView() { Width = 58, Height = 18, TabIndex = 0 };
            PageControl1.AddTab(pageAuthor, true);
            PageControl1.AddTab(pageOther, false);
            Add(PageControl1);

            Width = 60;
            Height = 21;
        }
    }
}
