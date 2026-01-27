using Terminal.Gui.Drawing;
using Terminal.Gui.Views;

namespace GKUI.Forms
{
    partial class AboutDlg
    {
        private Label lblProduct;
        private Label lblVersion;
        private Label lblCopyright;
        private Label lblMail;
        private Button btnClose;
        private Label lblProjSite;
        private Label lblForum;
        private Label lblChannel;

        private void InitializeComponent()
        {
            lblProduct = new Label { X = 1, Y = 1 };

            lblVersion = new Label { X = 1, Y = 2 };

            lblCopyright = new Label { X = 1, Y = 3 };

            var backColor = this.GetScheme().Normal.Background;
            var linkScheme = new Scheme() {
                Normal = new Attribute(Color.BrightBlue, backColor, TextStyle.Underline)
            };

            lblMail = new Label { Text = "mailto:gedkeeper@yandex.ru", X = 1, Y = 4 };
            lblMail.SetScheme(linkScheme);
            lblMail.MouseEvent += LabelMail_Click;

            lblProjSite = new Label { Text = "https://gedkeeper.net/", X = 1, Y = 5 };
            lblProjSite.SetScheme(linkScheme);
            lblProjSite.MouseEvent += LabelMail_Click;

            lblForum = new Label { Text = "https://gedkeeper.github.io/", X = 1, Y = 6 };
            lblForum.SetScheme(linkScheme);
            lblForum.MouseEvent += LabelMail_Click;

            lblChannel = new Label { Text = "https://gedkeeper.github.io/", X = 1, Y = 7 };
            lblChannel.SetScheme(linkScheme);
            lblChannel.MouseEvent += LabelMail_Click;

            Add(lblProduct, lblVersion, lblCopyright, lblChannel, lblForum, lblProjSite, lblMail);

            btnClose = new Button();
            btnClose.Accepted += CancelClickHandler;
            AddButton(btnClose);

            Width = 60;
            Height = 14;
        }
    }
}
