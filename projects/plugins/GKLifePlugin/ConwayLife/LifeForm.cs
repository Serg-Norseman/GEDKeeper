/*
 *  ULife, the old computer simulation of Life.
 *  Copyright (C) 1998 by Ian Lane (email: lanei@ideal.net.au)
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Sergey V. Zhdanovskih.
 */

using System;
using System.Windows.Forms;
using GKCore.Interfaces;

namespace GKLifePlugin.ConwayLife
{
    public partial class LifeForm : Form, ILocalizable
    {
        private static string PatternStabilisedTitle = "Стабильность образца";
        private static string RepeatingPattern = "Образец повторяется через каждые {0} поколений!";
        private static string StaticPattern = "Образец статичен!";

        private bool fIsMinimised;
        private ILangMan fLangMan;

        public LifeForm(ILangMan langMan)
        {
            InitializeComponent();

            fLangMan = langMan;

            cmpLife.AcceptMouseClicks = false;
            cmpLife.ShowGridLines = false;
        }

        private void Form_Load(object sender, EventArgs e)
        {
            SetLocale();
            UpdateMenusAndButtons();
        }

        private void Form_Resize(object sender, EventArgs e)
        {
            fIsMinimised = (WindowState == FormWindowState.Minimized);
        }

        public void SetLocale()
        {
            Text = fLangMan.LS(PLS.LifeGame);
            tbStep.Text = fLangMan.LS(PLS.Step);
            tbStart.Text = fLangMan.LS(PLS.Start);
            btnSetCells.Text = fLangMan.LS(PLS.SetCells);
            tbClear.Text = fLangMan.LS(PLS.Clear);
            tbRandomise.Text = fLangMan.LS(PLS.Random);
        }

        private void tbRandomise_Click(object sender, EventArgs e)
        {
            cmpLife.RandomCells();
        }

        private void tmrNextGeneration_Tick(object sender, EventArgs e)
        {
            if (fIsMinimised) return;

            int Periodicity = cmpLife.NextGeneration();
            if (Periodicity > 0) {
                tmrNextGeneration.Enabled = false;
                tbStart.Checked = false;
                UpdateMenusAndButtons();
                PatternStabilised(Periodicity);
            }
        }

        private void cmpLife_Change(object sender)
        {
            stlGeneration.Text = "Поколений: " + cmpLife.Generation.ToString();
            stlLivingCells.Text = "Живых клеток: " + cmpLife.LiveCellCount.ToString();
        }

        private void UpdateMenusAndButtons()
        {
            tbStep.Enabled = !tbStart.Checked && !btnSetCells.Checked;

            tbStart.Enabled = !btnSetCells.Checked;
            if (tbStart.Checked) {
                cmpLife.OnChange += cmpLife_Change;
                tbStart.Text = fLangMan.LS(PLS.Stop);
                tbStart.ToolTipText = "Остановка хода поколений";
            } else {
                cmpLife.OnChange -= cmpLife_Change;
                tbStart.Text = fLangMan.LS(PLS.Start);
                tbStart.ToolTipText = "Автоматическое прохождение поколений";
            }

            btnSetCells.Enabled = !tbStart.Checked;
            tbClear.Enabled = !tbStart.Checked;
            tbRandomise.Enabled = !tbStart.Checked;
            cmpLife.Cursor = cmpLife.AcceptMouseClicks ? Cursors.Hand : Cursors.Default;
        }

        private void PatternStabilised(int periodicity)
        {
            string msg = (periodicity == 1) ? StaticPattern : string.Format(RepeatingPattern, periodicity);
            MessageBox.Show(msg, PatternStabilisedTitle, MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
        }

        private void tbStep_Click(object sender, EventArgs e)
        {
            int Periodicity = cmpLife.NextGeneration();
            if (Periodicity > 0) PatternStabilised(Periodicity);
        }

        private void tbStart_Click(object sender, EventArgs e)
        {
            tmrNextGeneration.Enabled = tbStart.Checked;
            UpdateMenusAndButtons();
        }

        private void tbClear_Click(object sender, EventArgs e)
        {
            cmpLife.ClearCells();
        }

        private void tbSetCells_Click(object sender, EventArgs e)
        {
            cmpLife.AcceptMouseClicks = btnSetCells.Checked;
            cmpLife.ShowGridLines = cmpLife.AcceptMouseClicks;
        }
    }
}
