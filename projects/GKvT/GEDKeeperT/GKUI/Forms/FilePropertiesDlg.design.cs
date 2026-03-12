#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class FilePropertiesDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabView.Tab pageAuthor;
        private Label lblName;
        private Label lblAddress;
        private Label lblTelephone;
        private TextField txtName;
        private TextField txtTel;
        private TextView txtAddress;
        private TabView.Tab pageOther;
        private TabView PageControl1;
        private GKListView lvRecordStats;
        private ComboBox txtLanguage;
        private Label lblLanguage;

        private void InitializeComponent()
        {
            btnAccept = new Button() { Width = 16, TabIndex = 1 };
            btnAccept.Clicked += AcceptClickHandler;
            AddButton(btnAccept);

            btnCancel = new Button() { Width = 16, TabIndex = 2 };
            btnCancel.Clicked += CancelClickHandler;
            AddButton(btnCancel);

            lblName = new Label() { X = 1, Y = 1, TabIndex = 0 };
            lblTelephone = new Label() { X = 1, Y = 3, TabIndex = 2 };
            lblAddress = new Label() { X = 1, Y = 5, TabIndex = 4 };
            lblLanguage = new Label() { X = 1, Y = 11, TabIndex = 6 };

            txtName = new TextField() { X = 1, Y = 2, TabIndex = 1, Width = 54, Height = 1 };
            txtTel = new TextField() { X = 1, Y = 4, TabIndex = 3, Width = 54, Height = 1 };
            txtAddress = new TextView() { X = 1, Y = 6, TabIndex = 5, Width = 54, Height = 5, Multiline = true, AllowsReturn = true, AllowsTab = false };
            txtLanguage = new ComboBox() { X = 1, Y = 12, TabIndex = 7, Width = 54, Height = 2 };

            var container = new View();
            container.Add(lblName, lblTelephone, lblAddress, lblLanguage);
            container.Add(txtName, txtTel, txtAddress, txtLanguage);
            pageAuthor = new TabView.Tab() { View = container };

            lvRecordStats = new GKListView();
            lvRecordStats.Width = Dim.Fill();
            lvRecordStats.Height = Dim.Fill();
            pageOther = new TabView.Tab() { View = lvRecordStats };

            PageControl1 = new TabView() { Width = 58, Height = 18, TabIndex = 0 };
            PageControl1.AddTab(pageAuthor, true);
            PageControl1.AddTab(pageOther, false);
            Add(PageControl1);

            Width = 60;
            Height = 21;
        }
    }
}
