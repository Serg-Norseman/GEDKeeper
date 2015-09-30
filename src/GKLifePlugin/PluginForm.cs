using System;
using System.Windows.Forms;

using GKCore.Interfaces;

namespace GKLifePlugin
{
    public partial class PluginForm : Form
    {
    	private static string PatternStabilisedTitle = "Стабильность образца";
    	private static string RepeatingPattern = "Образец повторяется через каждые {0} поколений!";
    	private static string StaticPattern = "Образец статичен!";

    	private IPlugin plugin;
    	private bool fIsMinimised;

        public PluginForm(IPlugin plugin)
        {
            InitializeComponent();
            this.plugin = plugin;
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
        		tbStart.Text = "Стоп";
        		tbStart.ToolTipText = "Остановка хода поколений";
        	} else {
        		cmpLife.OnChange -= cmpLife_Change;
        		tbStart.Text = "Старт";
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

		void PluginForm_Load(object sender, EventArgs e)
		{
			this.UpdateMenusAndButtons();
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
    }
}
