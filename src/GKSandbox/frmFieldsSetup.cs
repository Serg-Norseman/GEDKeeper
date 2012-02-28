using System;
using System.Windows.Forms;

/// <summary>
/// Localization: unknown
/// CodeTransformation: need
/// </summary>

namespace GKSandbox
{
	public partial class frmFieldsSetup : Form
	{
		private ReportTableProperties TableProps;
		private TableHeader hx;

		public frmFieldsSetup(ReportTableProperties tableProps)
		{
			this.InitializeComponent();
			this.TableProps = tableProps;
		}

		void OnShown(object sender, EventArgs e)
		{
			this.listBox1.Items.Clear();
			this.listBox2.Items.Clear();
			this.listBox2.Items.Add("ID");
			this.listBox2.Items.Add("Firstname");
			this.listBox2.Items.Add("Lastname");
			this.listBox2.Items.Add("Sex");
			this.listBox2.Items.Add("FirstLastName");
			this.listBox2.Items.Add("Date of Birth");
			this.listBox2.Items.Add("Place of Birth");
			this.listBox2.Items.Add("Date of Baptism");
			this.listBox2.Items.Add("Place of Baptism");
			this.listBox2.Items.Add("Date of Death");
			this.listBox2.Items.Add("Place of Death");
			this.listBox2.Items.Add("Address");
			this.listBox2.Items.Add("Occupation");
			this.listBox2.Items.Add("Comments");
			this.listBox2.Items.Add("Parents");
			this.listBox2.Items.Add("Marriages");
			this.listBox2.Items.Add("Children");
			this.listBox2.Items.Add("Location");
			this.listBox2.Items.Add("Date of Register");
			float num = 0f;
			foreach (TableHeader current in this.TableProps.fields2)
			{
				this.listBox1.Items.Add(current.Name);
				int num2 = this.listBox2.Items.IndexOf(current.Name);
				num += current.Width;
				if (num2 > -1)
				{
					this.listBox2.Items.RemoveAt(num2);
				}
			}
			this.comboBox1.Items.Clear();
			this.comboBox1.Items.Add("Left");
			this.comboBox1.Items.Add("Center");
			this.comboBox1.Items.Add("Right");
			this.toolStripStatusLabel1.Text = "Sum of field widths: " + num.ToString() + "%";
		}

		void button3_Click(object sender, EventArgs e)
		{
			int num = this.listBox1.SelectedIndex;
			int selectedIndex = this.listBox2.SelectedIndex;
			if (selectedIndex < 0)
			{
				return;
			}
			if (num < 0)
			{
				num = this.listBox1.Items.Count;
			}
			TableHeader item = new TableHeader(this.listBox2.Items[selectedIndex].ToString(), 10f, 1);
			this.TableProps.fields2.Insert(num, item);
			this.listBox1.Items.Insert(num, this.listBox2.Items[selectedIndex].ToString());
			this.listBox2.Items.RemoveAt(selectedIndex);
			float num2 = 0f;
			foreach (TableHeader current in this.TableProps.fields2)
			{
				num2 += current.Width;
			}
			this.toolStripStatusLabel1.Text = "Sum of field widths: " + num2.ToString() + "%";
		}

		void button4_Click(object sender, EventArgs e)
		{
			int selectedIndex = this.listBox1.SelectedIndex;
			if (selectedIndex < 0)
			{
				return;
			}
			this.listBox2.Items.Add(this.listBox1.Items[selectedIndex]);
			this.TableProps.fields2.RemoveAt(selectedIndex);
			this.listBox1.Items.RemoveAt(selectedIndex);
			float num = 0f;
			foreach (TableHeader current in this.TableProps.fields2)
			{
				num += current.Width;
			}
			this.toolStripStatusLabel1.Text = "Sum of field widths: " + num.ToString() + "%";
		}

		void button1_Click(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.OK;
		}

		void OnSelectedIndexChangeList1(object sender, EventArgs e)
		{
			int selectedIndex = this.listBox1.SelectedIndex;
			if (selectedIndex < 0)
			{
				return;
			}
			string b = this.listBox1.Items[selectedIndex].ToString();
			foreach (TableHeader current in this.TableProps.fields2)
			{
				if (current.Name == b)
				{
					this.hx = current;
					break;
				}
			}
			if (this.hx != null)
			{
				this.textBox1.Text = this.hx.Name;
				this.textBox2.Text = this.hx.Alias;
				this.textBox3.Text = this.hx.Width.ToString();
				if (this.hx.Alignment == 0)
				{
					this.comboBox1.SelectedIndex = 0;
					return;
				}
				if (this.hx.Alignment == 1)
				{
					this.comboBox1.SelectedIndex = 1;
					return;
				}
				this.comboBox1.SelectedIndex = 2;
			}
		}

		void OnAliasChanged(object sender, EventArgs e)
		{
			if (this.hx != null)
			{
				this.hx.Alias = this.textBox2.Text;
			}
		}

		void OnSelectedIndexChangeCombo1(object sender, EventArgs e)
		{
			if (this.hx != null)
			{
				if (this.comboBox1.SelectedIndex == 0)
				{
					this.hx.Alignment = 0;
					return;
				}
				if (this.comboBox1.SelectedIndex == 1)
				{
					this.hx.Alignment = 1;
					return;
				}
				this.hx.Alignment = 2;
			}
		}

		void OnWidthChanged(object sender, EventArgs e)
		{
			float num = 0f;
			if (this.hx != null && float.TryParse(this.textBox3.Text, out num))
			{
				this.hx.Width = num;
			}
			num = 0f;
			foreach (TableHeader current in this.TableProps.fields2)
			{
				num += current.Width;
			}
			this.toolStripStatusLabel1.Text = "Sum of field widths: " + num.ToString() + "%";
		}

		void button5_Click(object sender, EventArgs e)
		{
			int selectedIndex = this.listBox1.SelectedIndex;
			if (selectedIndex < 1)
			{
				return;
			}
			if (this.listBox1.Items.Count > 1)
			{
				TableHeader TableHeader = this.TableProps.fields2[selectedIndex];
				this.TableProps.fields2.RemoveAt(selectedIndex);
				this.TableProps.fields2.Insert(selectedIndex - 1, TableHeader);
				this.listBox1.Items.RemoveAt(selectedIndex);
				this.listBox1.Items.Insert(selectedIndex - 1, TableHeader.Name);
				this.listBox1.SelectedIndex = selectedIndex - 1;
			}
		}

		void button6_Click(object sender, EventArgs e)
		{
			int selectedIndex = this.listBox1.SelectedIndex;
			if (selectedIndex < 0)
			{
				return;
			}
			if (selectedIndex == this.listBox1.Items.Count - 1)
			{
				return;
			}
			if (this.listBox1.Items.Count > 1)
			{
				TableHeader TableHeader = this.TableProps.fields2[selectedIndex];
				this.TableProps.fields2.RemoveAt(selectedIndex);
				this.TableProps.fields2.Insert(selectedIndex + 1, TableHeader);
				this.listBox1.Items.RemoveAt(selectedIndex);
				this.listBox1.Items.Insert(selectedIndex + 1, TableHeader.Name);
				this.listBox1.SelectedIndex = selectedIndex + 1;
			}
		}

		void button2_Click(object sender, EventArgs e)
		{
			//frmQuery frmQuery = new frmQuery(this.doc.ReportProperties.Table.fields2);
			//frmQuery.ShowDialog();
		}
	}
}
