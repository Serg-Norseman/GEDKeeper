using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Windows.Forms;
using GedCom551;
using GKCore;

namespace GKSandbox
{
	public class FamilyComboBox : UserControl
	{
		private TGEDCOMTree doc;
		private ComboBox comboBox1;

		/*public event OnFilledDelegate OnItemsChanged
		{
			[MethodImpl(MethodImplOptions.Synchronized)]
			add
			{
				this.OnItemsChanged = (OnFilledDelegate)Delegate.Combine(this.OnItemsChanged, value);
			}
			[MethodImpl(MethodImplOptions.Synchronized)]
			remove
			{
				this.OnItemsChanged = (OnFilledDelegate)Delegate.Remove(this.OnItemsChanged, value);
			}
		}**/

		public TGEDCOMTree Doc
		{
			set
			{
				this.doc = value;
			}
		}

		/*public TGEDCOMFamilyRecord Family
		{
			get
			{
				int id = ATA.LeftStringToInt(this.comboBox1.Text);
				return this.doc.GetFamily(id);
			}
		}*/

		public FamilyComboBox()
		{
			this.InitializeComponent();
			comboBox1.KeyDown += OnKeyDown;
			comboBox1.Enter += OnEnterForm;
		}

		private void OnKeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Return)
			{
				if (this.comboBox1.Text == "<Search>")
				{
					this.comboBox1.Text = "";
					e.Handled = true;
					return;
				}
				e.Handled = true;
				this.FindFamilies(this.comboBox1.Text);
			}
		}

		private void FindFamilies(string s)
		{
			this.comboBox1.Items.Clear();

			TGEDCOMRecord rec;
			ITreeEnumerator iEnum = doc.GetEnumerator(TGEDCOMRecordType.rtFamily);
			while (iEnum.MoveNext(out rec)) {
				TGEDCOMFamilyRecord fam = rec as TGEDCOMFamilyRecord;

				string longFamilyName = TGenEngine.aux_GetFamilyStr(fam);
				if (s == "" || longFamilyName.Contains(s))
				{
					this.comboBox1.Items.Add(longFamilyName);
				}
			}

			//this.OnItemsChanged(this, this.comboBox1.Items.Count);
			this.comboBox1.SelectionStart = this.comboBox1.Text.Length;
		}

		private void OnEnterForm(object sender, EventArgs e)
		{
			this.comboBox1.Text = "<Search>";
			this.comboBox1.Focus();
		}

		protected override void Dispose(bool disposing)
		{
			base.Dispose(disposing);
		}

		private void InitializeComponent()
		{
			this.comboBox1 = new ComboBox();
			base.SuspendLayout();
			this.comboBox1.FormattingEnabled = true;
			this.comboBox1.Location = new Point(3, 3);
			this.comboBox1.Name = "comboBox1";
			this.comboBox1.Size = new Size(288, 21);
			this.comboBox1.TabIndex = 0;
			base.AutoScaleDimensions = new SizeF(6f, 13f);
			base.AutoScaleMode = AutoScaleMode.Font;
			base.Controls.Add(this.comboBox1);
			base.Name = "FamilyComboBox";
			base.Size = new Size(294, 26);
			base.ResumeLayout(false);
		}
	}
}
