using System;
using Eto.Forms;
using Eto.Drawing;

namespace GKUI.Dialogs
{
    partial class AboutDlg
    {
        private Label lblProduct;
        private Label lblVersion;
        private Label lblCopyright;
        private LinkButton lblMail;
        private Button btnClose;
        private LinkButton lblProjSite;

        private void InitializeComponent()
        {
            lblProduct = new Label();
            lblProduct.Font = new Font("Times New Roman", 20.25F, FontStyle.Bold);
            lblProduct.Text = "lblProduct";

            lblVersion = new Label();
            lblVersion.Font = new Font("Times New Roman", 11.25F, FontStyle.Bold);
            lblVersion.Text = "lblVersion";

            btnClose = new Button();
            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(91, 24);
            btnClose.Text = "btnClose";
            btnClose.Click += (sender, e) => Close();

            lblCopyright = new Label();
            lblCopyright.Font = new Font("Times New Roman", 11.25F, FontStyle.Bold);
            lblCopyright.Text = "lblCopyright";

            lblMail = new LinkButton();
            lblMail.Font = new Font("Tahoma", 8.25F, FontStyle.Bold);
            lblMail.Text = "mailto:gedkeeper@yandex.ru";
            lblMail.Click += LabelMail_Click;

            lblProjSite = new LinkButton();
            lblProjSite.Font = new Font("Tahoma", 8.25F, FontStyle.Bold);
            lblProjSite.Text = "https://gedkeeper.github.io/";
            lblProjSite.Click += LabelMail_Click;

            ClientSize = new Size(383, 221);
            Title = "AboutDlg";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    lblProduct,
                    lblVersion,
                    lblCopyright,
                    null,
                    lblProjSite,
                    lblMail,
                    new TableRow(null, TableLayout.AutoSized(btnClose, centered: false))
                }
            };
        }
    }
}
