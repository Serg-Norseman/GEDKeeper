/*
 *  ULife
 *  Author: Ian Lane (email: lanei@ideal.net.au)
 *  Copyright (C) 1998 Ian Lane
 *
 *  Synopsis: A Delphi control which implements the old computer simulation
 *  of Life. Useful for about boxes, screen savers or even as the
 *  core of a "Life" application.
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Serg V. Zhdanovskih
 */

using System;
using System.Windows.Forms;

namespace ConwayLife
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
    			return this.cmpLife;
    		}
    	}
    	
        public LifeForm()
        {
            InitializeComponent();

            //cmpLife.ShowGridLines = true;
        }

        private void tbRandomise_Click(object sender, EventArgs e)
        {
        	cmpLife.RandomCells();
        }

        void tmrNextGeneration_Tick(object sender, EventArgs e)
		{
        	if (this.fIsMinimised) return;
        	
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

        	if (cmpLife.AcceptMouseClicks) {
        		cmpLife.Cursor = Cursors.Hand;
        	} else {
        		cmpLife.Cursor = Cursors.Default;
        	}
        }
        
        private void PatternStabilised(int Periodicity)
        {
        	string msg;
        	if (Periodicity == 1) {
        		msg = StaticPattern;
        	} else {
        		msg = string.Format(RepeatingPattern, Periodicity);
        	}
        	
        	MessageBox.Show(msg, PatternStabilisedTitle, MessageBoxButtons.OK, MessageBoxIcon.Asterisk);
        }
	
        void tbStep_Click(object sender, EventArgs e)
		{
        	int Periodicity = cmpLife.NextGeneration();
        	if (Periodicity > 0) PatternStabilised(Periodicity);
        }
	
        void tbStart_Click(object sender, EventArgs e)
		{
        	tmrNextGeneration.Enabled = tbStart.Checked;
        	UpdateMenusAndButtons();
        }

        void tbClear_Click(object sender, EventArgs e)
		{
        	cmpLife.ClearCells();
		}

		void PluginFormResize(object sender, EventArgs e)
		{
			this.fIsMinimised = (this.WindowState == FormWindowState.Minimized);
		}

		void tbSetCells_Click(object sender, EventArgs e)
		{
			cmpLife.AcceptMouseClicks = btnSetCells.Checked;
			cmpLife.ShowGridLines = cmpLife.AcceptMouseClicks;
		}

		void PluginForm_Load(object sender, EventArgs e)
		{
            this.SetLang();
			this.UpdateMenusAndButtons();
		}

		public void SetLang()
		{
			this.Text = Viewer.Options.LS_LifeGame;
			this.tbStep.Text = Viewer.Options.LS_Step;
			this.tbStart.Text = Viewer.Options.LS_Start;
			this.btnSetCells.Text = Viewer.Options.LS_SetCells;
			this.tbClear.Text = Viewer.Options.LS_Clear;
			this.tbRandomise.Text = Viewer.Options.LS_Random;
			this.tbOptions.Text = Viewer.Options.LS_Options;
		}
        
        void tbOptions_Click(object sender, EventArgs e)
        {
        	OptionsForm optsForm = new OptionsForm(cmpLife.Options, cmpLife.Rules);
        	if (optsForm.ShowDialog() == DialogResult.OK) {
        		tmrNextGeneration.Interval = cmpLife.Options.AnimationDelay;
        		
        		cmpLife.SetGridSize(cmpLife.Options.GridWidth, cmpLife.Options.GridHeight);
        	}
        }
    }
}
