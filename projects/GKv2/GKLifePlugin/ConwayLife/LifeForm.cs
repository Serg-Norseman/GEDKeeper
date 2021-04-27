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

namespace GKLifePlugin.ConwayLife
{
    public partial class LifeForm : Form
    {
        private static string PatternStabilisedTitle = "Стабильность образца";
        private static string RepeatingPattern = "Образец повторяется через каждые {0} поколений!";
        private static string StaticPattern = "Образец статичен!";

        private bool fIsMinimised;

        public LifeViewer Viewer
        {
            get {
                return cmpLife;
            }
        }
        
        public LifeForm()
        {
            InitializeComponent();

            cmpLife.AcceptMouseClicks = false;
            cmpLife.MaxNumberOfHistoryLevels = 10;
            cmpLife.GridHeight = LifeConsts.DefaultGridHeight;
            cmpLife.GridWidth = LifeConsts.DefaultGridWidth;
            cmpLife.ShowGridLines = false;
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
            tbOptions.Enabled = !tbStart.Checked && !btnSetCells.Checked;
            tbStep.Enabled = !tbStart.Checked && !btnSetCells.Checked;

            tbStart.Enabled = !btnSetCells.Checked;
            if (tbStart.Checked) {
                cmpLife.OnChange += cmpLife_Change;
                tbStart.Text = Viewer.Options.LS_Stop;
                tbStart.ToolTipText = "Остановка хода поколений";
            } else {
                cmpLife.OnChange -= cmpLife_Change;
                tbStart.Text = Viewer.Options.LS_Start;
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

        private void PluginFormResize(object sender, EventArgs e)
        {
            fIsMinimised = (WindowState == FormWindowState.Minimized);
        }

        private void tbSetCells_Click(object sender, EventArgs e)
        {
            cmpLife.AcceptMouseClicks = btnSetCells.Checked;
            cmpLife.ShowGridLines = cmpLife.AcceptMouseClicks;
        }

        private void PluginForm_Load(object sender, EventArgs e)
        {
            SetLang();
            UpdateMenusAndButtons();
        }

        public void SetLang()
        {
            Text = Viewer.Options.LS_LifeGame;
            tbStep.Text = Viewer.Options.LS_Step;
            tbStart.Text = Viewer.Options.LS_Start;
            btnSetCells.Text = Viewer.Options.LS_SetCells;
            tbClear.Text = Viewer.Options.LS_Clear;
            tbRandomise.Text = Viewer.Options.LS_Random;
            tbOptions.Text = Viewer.Options.LS_Options;
        }
        
        private void tbOptions_Click(object sender, EventArgs e)
        {
            using (OptionsForm optsForm = new OptionsForm(cmpLife.Options, cmpLife.Rules)) {
                if (optsForm.ShowDialog() == DialogResult.OK) {
                    tmrNextGeneration.Interval = cmpLife.Options.AnimationDelay;

                    cmpLife.SetGridSize(cmpLife.Options.GridWidth, cmpLife.Options.GridHeight);
                }
            }
        }
    }
}
