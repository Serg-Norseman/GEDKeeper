using GKCore;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmScriptDaemon : Form
	{
		private ToolBar ToolBar1;
		private ToolBarButton btnLoadScript;
		private ToolBarButton ToolButton2;
		private ToolBarButton btnRun;
		private TextBox mmDebugOutput;
		private TextBox mmScriptText;
		private OpenFileDialog OpenDialog1;
		private ToolBarButton btnSaveScript;
		private SaveFileDialog SaveDialog1;
		private ToolBarButton btnNewScript;
		private TfmBase FBase;
		private string FFileName;
		private bool FModified;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public string FileName
		{
			get
			{
				return this.FFileName;
			}
			set
			{
				this.SetFileName(value);
			}
		}
		[Browsable(false)]
		public bool Modified
		{
			get
			{
				return this.FModified;
			}
			set
			{
				this.SetModified(value);
			}
		}
		private bool CheckModified()
		{
			bool Result = true;
			if (this.Modified)
			{
				DialogResult dialogResult = MessageBox.Show(GKL.LSList[69], "GEDKeeper", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
				if (dialogResult != DialogResult.Cancel)
				{
					if (dialogResult == DialogResult.Yes)
					{
						this.SaveScript();
					}
				}
				else
				{
					Result = false;
				}
			}
			return Result;
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
		private void InitializeComponent()
		{
			this.ToolBar1 = new ToolBar();
			this.btnNewScript = new ToolBarButton();
			this.btnLoadScript = new ToolBarButton();
			this.btnSaveScript = new ToolBarButton();
			this.ToolButton2 = new ToolBarButton();
			this.btnRun = new ToolBarButton();
			this.mmDebugOutput = new TextBox();
			this.mmScriptText = new TextBox();
			this.OpenDialog1 = new OpenFileDialog();
			this.SaveDialog1 = new SaveFileDialog();
			base.SuspendLayout();
			this.ToolBar1.Appearance = ToolBarAppearance.Flat;
			ToolBar.ToolBarButtonCollection arg_DE_0 = this.ToolBar1.Buttons;
			ToolBarButton[] array = null;
			ToolBarButton[] array2 = array;
			ToolBarButton[] array3;
			ToolBarButton[] expr_95 = array3 = new ToolBarButton[5];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 5)
				{
					num = 5;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_95;
			array[0] = this.btnNewScript;
			array[1] = this.btnLoadScript;
			array[2] = this.btnSaveScript;
			array[3] = this.ToolButton2;
			array[4] = this.btnRun;
			arg_DE_0.AddRange(array);
			this.ToolBar1.DropDownArrows = true;
			this.ToolBar1.Location = new Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.ShowToolTips = true;
			this.ToolBar1.Size = new Size(712, 28);
			this.ToolBar1.TabIndex = 0;
			this.ToolBar1.ButtonClick += new ToolBarButtonClickEventHandler(this.ToolBar1_ButtonClick);
			this.btnNewScript.ImageIndex = 0;
			this.btnNewScript.ToolTipText = "Новый скрипт";
			this.btnLoadScript.ImageIndex = 1;
			this.btnLoadScript.ToolTipText = "Загрузить скрипт";
			this.btnSaveScript.ImageIndex = 2;
			this.btnSaveScript.ToolTipText = "Сохранить скрипт";
			this.ToolButton2.Style = ToolBarButtonStyle.Separator;
			this.btnRun.ImageIndex = 33;
			this.btnRun.ToolTipText = "Выполнить";
			this.mmDebugOutput.Location = new Point(0, 286);
			this.mmDebugOutput.Multiline = true;
			this.mmDebugOutput.Name = "mmDebugOutput";
			this.mmDebugOutput.ReadOnly = true;
			this.mmDebugOutput.Size = new Size(712, 148);
			this.mmDebugOutput.TabIndex = 1;
			this.mmDebugOutput.Text = "";
			this.mmScriptText.Location = new Point(0, 30);
			this.mmScriptText.Multiline = true;
			this.mmScriptText.Name = "mmScriptText";
			this.mmScriptText.Size = new Size(712, 253);
			this.mmScriptText.TabIndex = 2;
			this.mmScriptText.Text = "";
			this.mmScriptText.TextChanged += new EventHandler(this.mmScriptText_TextChanged);
			this.OpenDialog1.Filter = "Скрипты|*.lua";
			this.SaveDialog1.DefaultExt = "lua";
			this.SaveDialog1.Filter = "Скрипты|*.lua";
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(712, 434);
			base.Controls.Add(this.ToolBar1);
			base.Controls.Add(this.mmDebugOutput);
			base.Controls.Add(this.mmScriptText);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.Name = "TfmScriptDaemon";
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "ScriptDaemon";
			base.Closing += new CancelEventHandler(this.TfmScriptDaemon_Closing);
			base.ResumeLayout(false);
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
				StreamReader strd = new StreamReader(File.OpenRead(this.SaveDialog1.FileName));
				this.mmScriptText.Text = strd.ReadToEnd();
				this.FileName = this.OpenDialog1.FileName;
				this.Modified = false;
			}
		}
		private void SaveScript()
		{
			this.SaveDialog1.FileName = this.FileName;
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
			{
				StreamWriter strd = new StreamWriter(this.SaveDialog1.FileName, false);
				strd.Write(this.mmScriptText.Text);
				this.FileName = this.SaveDialog1.FileName;
				this.Modified = false;
			}
		}
		private void Run()
		{
			this.mmDebugOutput.Clear();
			try
			{
				TScriptEngine scr_engine = new TScriptEngine();
				try
				{
					scr_engine.lua_run(this.mmScriptText.Text, this.Base, this.mmDebugOutput);
				}
				finally
				{
					scr_engine.Free();
				}
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKScriptDaemon.Run(): " + E.Message);
			}
		}
		private void TfmScriptDaemon_Closing(object sender, CancelEventArgs e)
		{
			if (!this.CheckModified())
			{
				e.Cancel = true;
			}
			else
			{
				e.Cancel = false;
			}
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
			this.ToolBar1.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.NewScript();
			this.SetLang();
		}

		public void SetLang()
		{
		}
	}
}
