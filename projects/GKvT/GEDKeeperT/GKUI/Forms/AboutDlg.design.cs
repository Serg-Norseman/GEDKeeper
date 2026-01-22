using Terminal.Gui;

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

            var backColor = this.ColorScheme.Normal.Background;
            var linkScheme = new ColorScheme() {
                Normal = Attribute.Make(Color.Blue, backColor)
            };

            lblMail = new Label { Text = "mailto:gedkeeper@yandex.ru", X = 1, Y = 4, ColorScheme = linkScheme };
            lblMail.MouseClick += LabelMail_Click;

            lblProjSite = new Label { Text = "https://gedkeeper.net/", X = 1, Y = 5, ColorScheme = linkScheme };
            lblProjSite.MouseClick += LabelMail_Click;

            lblForum = new Label { Text = "https://gedkeeper.github.io/", X = 1, Y = 6, ColorScheme = linkScheme };
            lblForum.MouseClick += LabelMail_Click;

            lblChannel = new Label { Text = "https://gedkeeper.github.io/", X = 1, Y = 7, ColorScheme = linkScheme };
            lblChannel.MouseClick += LabelMail_Click;

            Add(lblProduct, lblVersion, lblCopyright, lblChannel, lblForum, lblProjSite, lblMail);

            btnClose = new Button();
            btnClose.MouseClick += CancelClickHandler;
            AddButton(btnClose);

            Width = 60;
            Height = 12;
        }
    }
}
