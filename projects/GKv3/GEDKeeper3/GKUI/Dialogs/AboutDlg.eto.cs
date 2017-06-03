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
            lblVersion = new Label();
            btnClose = new Button();
            lblCopyright = new Label();
            lblMail = new LinkButton();
            lblProjSite = new LinkButton();

            lblProduct.Font = new Font("Times New Roman", 20.25F, FontStyle.Bold);
            lblProduct.Text = "lblProduct";

            lblVersion.Font = new Font("Times New Roman", 11.25F, FontStyle.Bold);
            lblVersion.Text = "lblVersion";

            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(91, 24);
            btnClose.Text = "btnClose";
            btnClose.Click += (sender, e) => Close();

            lblCopyright.Font = new Font("Times New Roman", 11.25F, FontStyle.Bold);
            lblCopyright.Text = "lblCopyright";

            lblMail.Font = new Font("Tahoma", 8.25F, FontStyle.Bold);
            lblMail.Text = "mailto:gedkeeper@yandex.ru";
            lblMail.Click += LabelMail_Click;

            lblProjSite.Font = new Font("Tahoma", 8.25F, FontStyle.Bold);
            lblProjSite.Text = "https://gedkeeper.github.io/";
            lblProjSite.Click += LabelMail_Click;

            ClientSize = new Size(383, 221);
            Maximizable = false;
            Minimizable = false;
            Resizable = false;
            ShowInTaskbar = false;
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
