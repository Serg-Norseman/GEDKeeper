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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Interfaces;

namespace GKLifePlugin.ConwayLife
{
    public partial class LifeForm : Form, ILocalizable
    {
        private static string PatternStabilisedTitle = "Стабильность образца";
        private static string RepeatingPattern = "Образец повторяется через каждые {0} поколений!";
        private static string StaticPattern = "Образец статичен!";

        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private ITimer tmrNextGeneration;

        private ButtonToolItem tbStep;
        private CheckToolItem tbStart;
        private CheckToolItem btnSetCells;
        private ButtonToolItem tbClear;
        private ButtonToolItem tbRandomise;
        private LifeViewer cmpLife;
        private Label stlGeneration;
        private Label stlLivingCells;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private bool fIsMinimised;
        private ILangMan fLangMan;

        public LifeForm(ILangMan langMan)
        {
            XamlReader.Load(this);

            fLangMan = langMan;

            cmpLife.AcceptMouseClicks = false;
            cmpLife.ShowGridLines = false;

            tmrNextGeneration = AppHost.Instance.CreateTimer(LifeGrid.DefaultAnimationDelay, tmrNextGeneration_Tick);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (tmrNextGeneration != null)
                    tmrNextGeneration.Dispose();
            }
            base.Dispose(disposing);
        }

        private void Form_Shown(object sender, EventArgs e)
        {
            SetLocale();
            UpdateMenusAndButtons();
        }

        private void Form_Resize(object sender, EventArgs e)
        {
            fIsMinimised = (WindowState == WindowState.Minimized);
        }

        public void SetLocale()
        {
            Title = fLangMan.LS(PLS.LSID_LifeGame);
            tbStep.Text = fLangMan.LS(PLS.LSID_Step);
            tbStart.Text = fLangMan.LS(PLS.LSID_Start);
            btnSetCells.Text = fLangMan.LS(PLS.LSID_SetCells);
            tbClear.Text = fLangMan.LS(PLS.LSID_Clear);
            tbRandomise.Text = fLangMan.LS(PLS.LSID_Random);
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
                tbStart.Text = fLangMan.LS(PLS.LSID_Stop);
                tbStart.ToolTip = "Остановка хода поколений";
            } else {
                cmpLife.OnChange -= cmpLife_Change;
                tbStart.Text = fLangMan.LS(PLS.LSID_Start);
                tbStart.ToolTip = "Автоматическое прохождение поколений";
            }

            btnSetCells.Enabled = !tbStart.Checked;
            tbClear.Enabled = !tbStart.Checked;
            tbRandomise.Enabled = !tbStart.Checked;
            cmpLife.Cursor = cmpLife.AcceptMouseClicks ? Cursors.Pointer : Cursors.Default;
        }

        private void PatternStabilised(int periodicity)
        {
            string msg = (periodicity == 1) ? StaticPattern : string.Format(RepeatingPattern, periodicity);
            AppHost.StdDialogs.ShowWarning(msg, PatternStabilisedTitle);
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
