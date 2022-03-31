using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class AboutDlg
    {
        private Label lblProduct;
        private Label lblVersion;
        private Label lblCopyright;
        private LinkButton lblMail;
        private Button btnClose;
        private LinkButton lblProjSite;
        private LinkButton lblForum;
        private LinkButton lblChannel;

        private void InitializeComponent()
        {
            btnClose = new Button();
            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(130, 26);
            btnClose.Text = "btnClose";
            btnClose.Click += CancelClickHandler;

            lblProduct = new Label();
            lblProduct.Font = UIHelper.GetDefaultFont(20.25F, FontStyle.Bold);
            lblProduct.Text = "lblProduct";

            lblVersion = new Label();
            lblVersion.Font = UIHelper.GetDefaultFont(11.25F, FontStyle.Bold);
            lblVersion.Text = "lblVersion";

            lblCopyright = new Label();
            lblCopyright.Font = UIHelper.GetDefaultFont(11.25F, FontStyle.Bold);
            lblCopyright.Text = "lblCopyright";

            lblMail = new LinkButton();
            lblMail.Font = UIHelper.GetDefaultFont(9.0f, FontStyle.Bold);
            lblMail.Text = "mailto:gedkeeper@yandex.ru";
            lblMail.Click += LabelMail_Click;

            lblProjSite = new LinkButton();
            lblProjSite.Font = UIHelper.GetDefaultFont(9.0f, FontStyle.Bold);
            lblProjSite.Text = "https://gedkeeper.net/";
            lblProjSite.Click += LabelMail_Click;

            lblForum = new LinkButton();
            lblForum.Font = UIHelper.GetDefaultFont(9.0f, FontStyle.Bold);
            lblForum.Text = "https://gedkeeper.net/";
            lblForum.Click += LabelMail_Click;

            lblChannel = new LinkButton();
            lblChannel.Font = UIHelper.GetDefaultFont(9.0f, FontStyle.Bold);
            lblChannel.Text = "https://gedkeeper.net/";
            lblChannel.Click += LabelMail_Click;

            Content = new DefTableLayout {
                Rows = {
                    lblProduct,
                    lblVersion,
                    lblCopyright,
                    null,
                    lblProjSite,
                    lblMail,
                    lblForum,
                    lblChannel,
                    UIHelper.MakeDialogFooter(null, btnClose)
                }
            };

            Title = "AboutDlg";

            SetPredefProperties(360, 220, false);
        }
    }
}
