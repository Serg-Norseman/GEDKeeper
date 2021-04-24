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
    /// <summary>
    /// 
    /// </summary>
    public partial class OptionsForm : Form
    {
        private readonly LifeOptions fOptions;
        private readonly LifeRules fRules;

        public OptionsForm(LifeOptions options, LifeRules rules)
        {
            InitializeComponent();

            fOptions = options;
            fRules = rules;

            edtGridHeight.Minimum = LifeConsts.MinGridHeight;
            edtGridHeight.Maximum = LifeConsts.MaxGridHeight;

            edtGridWidth.Minimum = LifeConsts.MinGridWidth;
            edtGridWidth.Maximum = LifeConsts.MaxGridWidth;
        }

        private void btnApply_Click(object sender, EventArgs e)
        {
            fOptions.GridHeight = (int)edtGridHeight.Value;
            fOptions.GridWidth = (int)edtGridWidth.Value;
            fOptions.BackgroundColor = lblColourBackground.BackColor;
            fOptions.LivingCellColor = lblColourLivingCells.BackColor;
            fOptions.AnimationDelay = (int)edtAnimationDelay.Value;

            /*if Modified then SaveToRegistry*/

            fRules.SetLiveCells(0, chkLiveCell0.Checked);
            fRules.SetLiveCells(1, chkLiveCell1.Checked);
            fRules.SetLiveCells(2, chkLiveCell2.Checked);
            fRules.SetLiveCells(3, chkLiveCell3.Checked);
            fRules.SetLiveCells(4, chkLiveCell4.Checked);
            fRules.SetLiveCells(5, chkLiveCell5.Checked);
            fRules.SetLiveCells(6, chkLiveCell6.Checked);
            fRules.SetLiveCells(7, chkLiveCell7.Checked);
            fRules.SetLiveCells(8, chkLiveCell8.Checked);

            fRules.SetDeadCells(0, chkDeadCell0.Checked);
            fRules.SetDeadCells(1, chkDeadCell1.Checked);
            fRules.SetDeadCells(2, chkDeadCell2.Checked);
            fRules.SetDeadCells(3, chkDeadCell3.Checked);
            fRules.SetDeadCells(4, chkDeadCell4.Checked);
            fRules.SetDeadCells(5, chkDeadCell5.Checked);
            fRules.SetDeadCells(6, chkDeadCell6.Checked);
            fRules.SetDeadCells(7, chkDeadCell7.Checked);
            fRules.SetDeadCells(8, chkDeadCell8.Checked);

            /*if Modified then SaveToRegistry*/
        }

        private void btnRestoreGeneralDefaults_Click(object sender, EventArgs e)
        {
            fOptions.RestoreDefaults();
            UpdateGeneral();
        }

        private void btnRestoreRuleDefaults_Click(object sender, EventArgs e)
        {
            fRules.RestoreDefaults();
            UpdateRules();
        }

        private void OptionsForm_Load(object sender, EventArgs e)
        {
            UpdateGeneral();
            UpdateRules();
        }

        private void UpdateGeneral()
        {
            edtGridWidth.Value = fOptions.GridWidth;
            edtGridHeight.Value = fOptions.GridHeight;
            lblColourBackground.BackColor = fOptions.BackgroundColor;
            lblColourLivingCells.BackColor = fOptions.LivingCellColor;
            edtAnimationDelay.Value = fOptions.AnimationDelay;
        }

        private void UpdateRules()
        {
            chkLiveCell0.Checked = fRules.GetLiveCells(0);
            chkLiveCell1.Checked = fRules.GetLiveCells(1);
            chkLiveCell2.Checked = fRules.GetLiveCells(2);
            chkLiveCell3.Checked = fRules.GetLiveCells(3);
            chkLiveCell4.Checked = fRules.GetLiveCells(4);
            chkLiveCell5.Checked = fRules.GetLiveCells(5);
            chkLiveCell6.Checked = fRules.GetLiveCells(6);
            chkLiveCell7.Checked = fRules.GetLiveCells(7);
            chkLiveCell8.Checked = fRules.GetLiveCells(8);

            chkDeadCell0.Checked = fRules.GetDeadCells(0);
            chkDeadCell1.Checked = fRules.GetDeadCells(1);
            chkDeadCell2.Checked = fRules.GetDeadCells(2);
            chkDeadCell3.Checked = fRules.GetDeadCells(3);
            chkDeadCell4.Checked = fRules.GetDeadCells(4);
            chkDeadCell5.Checked = fRules.GetDeadCells(5);
            chkDeadCell6.Checked = fRules.GetDeadCells(6);
            chkDeadCell7.Checked = fRules.GetDeadCells(7);
            chkDeadCell8.Checked = fRules.GetDeadCells(8);
        }

        private void lblColours_Click(object sender, EventArgs e)
        {
            Label pan = (sender as Label);

            colorDialog1.FullOpen = true;
            colorDialog1.Color = pan.BackColor;
            if (colorDialog1.ShowDialog() == DialogResult.OK) {
                pan.BackColor = colorDialog1.Color;
            }
        }
    }
}
