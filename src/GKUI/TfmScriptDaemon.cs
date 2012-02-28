using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;
using GKCore;
using GKSys;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKUI
{
	public partial class TfmScriptDaemon : Form
	{
		private TfmBase FBase;
		private string FFileName;
		private bool FModified;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public string FileName
		{
			get { return this.FFileName; }
			set { this.SetFileName(value); }
		}

		public bool Modified
		{
			get { return this.FModified; }
			set { this.SetModified(value); }
		}

		private bool CheckModified()
		{
			bool result = true;

			if (this.Modified)
			{
				DialogResult dialogResult = MessageBox.Show(LangMan.LSList[69], "GEDKeeper2", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);

				switch (dialogResult) {
					case DialogResult.Yes:
						this.SaveScript();
						break;
					case DialogResult.No:
						break;
					case DialogResult.Cancel:
						result = false;
						break;
				}
			}

			return result;
		}

		private void SetFileName([In] string Value)
		{
			this.FFileName = Value;
			this.SetTitle();
		}

		private void SetModified([In] bool Value)
		{
			this.FModified = Value;
			this.SetTitle();
		}

		private void SetTitle()
		{
			this.Text = Path.GetFileName(this.FFileName);
			if (this.FModified)
			{
				this.Text = "* " + this.Text;
			}
		}

		private void NewScript()
		{
			if (this.CheckModified())
			{
				this.mmScriptText.Clear();
				this.FileName = "unknown.lua";
				this.Modified = false;
			}
		}

		private void LoadScript()
		{
			if (this.CheckModified() && this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				using (StreamReader strd = new StreamReader(File.OpenRead(this.OpenDialog1.FileName), Encoding.GetEncoding(1251)))
				{
					this.mmScriptText.Text = strd.ReadToEnd();
					this.FileName = this.OpenDialog1.FileName;
					this.Modified = false;
					strd.Close();
				}
			}
		}

		private void SaveScript()
		{
			this.SaveDialog1.FileName = this.FileName;
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
			{
				using (StreamWriter strd = new StreamWriter(this.SaveDialog1.FileName, false, Encoding.GetEncoding(1251)))
				{
					strd.Write(this.mmScriptText.Text);
					this.FileName = this.SaveDialog1.FileName;
					this.Modified = false;
					strd.Close();
				}
			}
		}

		private void Run()
		{
			try
			{
				this.mmDebugOutput.Clear();
				using (TScriptEngine scr_engine = new TScriptEngine()) {
					scr_engine.lua_run(this.mmScriptText.Text, this.Base, this.mmDebugOutput);
				}
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("GKScriptDaemon.Run(): " + E.Message);
			}
		}

		private void TfmScriptDaemon_Closing(object sender, CancelEventArgs e)
		{
			e.Cancel = !this.CheckModified();
		}

		private void mmScriptText_TextChanged(object sender, EventArgs e)
		{
			this.Modified = true;
		}

		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (object.Equals(e.Button, this.btnNewScript))
			{
				this.NewScript();
			}
			if (object.Equals(e.Button, this.btnLoadScript))
			{
				this.LoadScript();
			}
			if (object.Equals(e.Button, this.btnSaveScript))
			{
				this.SaveScript();
			}
			if (object.Equals(e.Button, this.btnRun))
			{
				this.Run();
			}
		}

		public TfmScriptDaemon(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.ToolBar1.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.NewScript();
			this.SetLang();
		}

		public void SetLang()
		{
		}
	}
}
