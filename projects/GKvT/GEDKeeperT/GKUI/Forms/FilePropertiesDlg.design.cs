using GKUI.Components;
using Terminal.Gui.Drawing;
using Terminal.Gui.ViewBase;
using Terminal.Gui.Views;

namespace GKUI.Forms
{
    partial class FilePropertiesDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Tab pageAuthor;
        private Label lblName;
        private Label lblAddress;
        private Label lblTelephone;
        private TextField txtName;
        private TextField txtTel;
        private TextView txtAddress;
        private Tab pageOther;
        private TabView PageControl1;
        private GKListView lvRecordStats;
        private ComboBox txtLanguage;
        private Label lblLanguage;

        private void InitializeComponent()
        {
            lblName = new Label() { X = 1, Y = 1 };
            lblTelephone = new Label() { X = 1, Y = 3 };
            lblAddress = new Label() { X = 1, Y = 5 };
            lblLanguage = new Label() { X = 1, Y = 11 };

            txtName = new TextField() { X = 1, Y = 2, Width = 54, Height = 1 };
            txtTel = new TextField() { X = 1, Y = 4, Width = 54, Height = 1 };
            txtAddress = new TextView() { X = 1, Y = 6, Width = 54, Height = 5, Multiline = true };
            txtLanguage = new ComboBox() { X = 1, Y = 12, Width = 54, Height = 1 };

            var container = new View();
            container.Border.BorderStyle = LineStyle.None;
            container.Height = Dim.Fill();
            container.Width = Dim.Fill();
            container.Add(lblName, lblTelephone, lblAddress, lblLanguage);
            container.Add(txtName, txtTel, txtAddress, txtLanguage);

            pageAuthor = new Tab() { View = container };

            lvRecordStats = new GKListView() { FullRowSelect = true, MultiSelect = false };
            lvRecordStats.Width = Dim.Fill();
            lvRecordStats.Height = Dim.Fill();

            pageOther = new Tab() { View = lvRecordStats };

            PageControl1 = new TabView() { Width = 58, Height = 18 };
            PageControl1.AddTab(pageAuthor, true);
            PageControl1.AddTab(pageOther, false);
            Add(PageControl1);

            btnAccept = new Button() { };
            btnAccept.Accepted += AcceptClickHandler;
            AddButton(btnAccept);

            btnCancel = new Button() { };
            btnCancel.Accepted += CancelClickHandler;
            AddButton(btnCancel);

            Width = 62;
            Height = 24;
        }
    }
}
