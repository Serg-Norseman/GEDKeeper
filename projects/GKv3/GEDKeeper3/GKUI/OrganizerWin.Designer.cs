using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI
{
    partial class OrganizerWin
    {
        private TabControl tabsData;
        private TabPage pageAddresses;
        private TabPage pageTelephones;
        private TabPage pageMails;
        private TabPage pageWebs;

        private void InitializeComponent()
        {
            tabsData = new TabControl();
            pageAddresses = new TabPage();
            pageTelephones = new TabPage();
            pageMails = new TabPage();
            pageWebs = new TabPage();
            tabsData.SuspendLayout();
            SuspendLayout();

            tabsData.Controls.Add(pageAddresses);
            tabsData.Controls.Add(pageTelephones);
            tabsData.Controls.Add(pageMails);
            tabsData.Controls.Add(pageWebs);
            tabsData.Dock = DockStyle.Fill;
            tabsData.Location = new Point(0, 0);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(785, 539);

            pageAddresses.Location = new Point(4, 26);
            pageAddresses.Size = new Size(777, 509);
            pageAddresses.Text = "pageAddresses";

            pageTelephones.Location = new Point(4, 26);
            pageTelephones.Size = new Size(777, 509);
            pageTelephones.Text = "pageTelephones";

            pageMails.Location = new Point(4, 26);
            pageMails.Size = new Size(777, 509);
            pageMails.Text = "pageMails";

            pageWebs.Location = new Point(4, 26);
            pageWebs.Size = new Size(777, 509);
            pageWebs.Text = "pageWebs";

            ClientSize = new Size(785, 539);
            Controls.Add(tabsData);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "OrganizerWin";
            Load += OrganizerWin_Load;
            KeyDown += OrganizerWin_KeyDown;
            tabsData.ResumeLayout();
            ResumeLayout();
        }
    }
}
