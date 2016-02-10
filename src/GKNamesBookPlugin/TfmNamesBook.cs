using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using BSLib;
using GKCommon.GEDCOM;

/// <summary>
/// 
/// </summary>

namespace GKNamesBookPlugin
{
	public class GKComboItem
	{
		public readonly string Caption;
		public readonly object Data;

		public GKComboItem(string caption, object data)
		{
			this.Caption = caption;
			this.Data = data;
		}

		public override string ToString()
		{
			return this.Caption;
		}
	}

	public partial class TfmNamesBook : Form
	{
		private class NameRecord
		{
			public string Name;
			public string Desc;
			public GEDCOMSex Sex;
			public int ChIndex;
		}

		private readonly Plugin fPlugin;
		private readonly List<NameRecord> fNames;
		private readonly StringList fChurchFNames;
		private readonly StringList fChurchMNames;

        public TfmNamesBook(Plugin plugin) : base()
        {
            this.InitializeComponent();

            this.fPlugin = plugin;
            
            Screen scr = Screen.PrimaryScreen;
            this.Location = new Point(scr.WorkingArea.Width - this.Width - 10, (scr.WorkingArea.Height - this.Height) / 2);

            this.fNames = new List<NameRecord>();
            this.fChurchFNames = new StringList();
            this.fChurchMNames = new StringList();

            this.PrepareList();
            this.UpdateList();

            this.Text = this.fPlugin.LangMan.LS(NLS.LSID_MINamesBook);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fChurchFNames.Dispose();
                this.fChurchMNames.Dispose();
                //this.fNames.Dispose();
            }
            base.Dispose(disposing);
        }

        private void TfmNamesBook_Load(object sender, EventArgs e)
        {
        	this.fPlugin.Host.WidgetShow(this.fPlugin);
        }

		private void TfmNamesBook_Closed(object sender, EventArgs e)
		{
			this.fPlugin.Host.WidgetClose(this.fPlugin);
		}

		private void cbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			int idx = this.cbNames.SelectedIndex;
			if (idx >= 0 && idx < this.cbNames.Items.Count)
			{
                GKComboItem item = (GKComboItem)this.cbNames.Items[idx];
				NameRecord rec = item.Data as NameRecord;

				this.mmDesc.Text = "";
				this.mmDesc.AppendText(rec.Name + "\r\n");
				this.mmDesc.AppendText(rec.Desc + "\r\n");

                if (rec.ChIndex >= 0)
				{
					this.mmDesc.AppendText("\r\n");
					this.mmDesc.AppendText("Святцы:\r\n");
					
                    StringList lst = null;
					if (rec.Sex == GEDCOMSex.svMale)
					{
						lst = this.fChurchMNames;
					}
					else
					{
						if (rec.Sex == GEDCOMSex.svFemale)
						{
							lst = this.fChurchFNames;
						}
					}

					int num = lst.Count;
					for (int i = rec.ChIndex + 1; i < num; i++)
					{
						string st = lst[i].Trim();
						if (st[0] == '-')
						{
							break;
						}
						st = st.Remove(0, 1);
						this.mmDesc.AppendText(st + "\r\n");
					}
				}
			}
		}

		private static bool ExtractFlags(ref string st)
		{
			bool res = (st != null);
			if (res) {
				res = (st.Length >= 2 && st[0] == '[' && st[st.Length - 1] == ']');
				if (res) {
					st = st.Substring(1, st.Length - 2);
				}
			}
			return res;
		}

		/*private static System.Resources.ResourceManager resourceMan;
		
		internal static System.Resources.ResourceManager ResourceManager {
			get {
				if (object.ReferenceEquals(resourceMan, null)) {
					System.Resources.ResourceManager temp = new System.Resources.ResourceManager("GKResources", typeof(GKResources).Assembly);
					resourceMan = temp;
				}
				return resourceMan;
			}
		}
		
		internal static byte[] book_names {
			get {
				object obj = ResourceManager.GetObject("book_names", resourceCulture);
				return ((byte[])(obj));
			}
		}
		
		internal static byte[] book_names_cf {
			get {
				object obj = ResourceManager.GetObject("book_names_cf", resourceCulture);
				return ((byte[])(obj));
			}
		}
		
		internal static byte[] book_names_cm {
			get {
				object obj = ResourceManager.GetObject("book_names_cm", resourceCulture);
				return ((byte[])(obj));
			}
		}*/

		private void PrepareList()
		{
		    using (MemoryStream memStream = new MemoryStream(NBResources.book_names))
		    {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1)
                    {
                        string ns = strd.ReadLine().Trim();
                        if (ns != "")
                        {
                            string[] toks = ns.Split('/');
                            if (toks.Length >= 3)
                            {
                                NameRecord rec = new NameRecord();

                                rec.Name = toks[0].Trim();
                                rec.Desc = toks[2].Trim();
                                string st = toks[1].Trim();

                                if (ExtractFlags(ref st))
                                {
                                    char c = st[0];
                                    switch (c)
                                    {
                                        case 'f':
                                            rec.Sex = GEDCOMSex.svFemale;
                                            break;
                                        case 'm':
                                            rec.Sex = GEDCOMSex.svMale;
                                            break;
                                    }
                                }

                                this.fNames.Add(rec);
                            }
                        }
                    }
                }
            }

            using (MemoryStream memStream = new MemoryStream(NBResources.book_names_cf))
            {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
			    {
				    while (strd.Peek() != -1) {
					    string ns = strd.ReadLine().Trim();
					    this.fChurchFNames.Add(ns);
				    }
			    }
            }

            using (MemoryStream memStream = new MemoryStream(NBResources.book_names_cm))
		    {
                using (StreamReader strd = new StreamReader(memStream, Encoding.GetEncoding(1251)))
                {
                    while (strd.Peek() != -1)
                    {
                        string ns = strd.ReadLine().Trim();
                        this.fChurchMNames.Add(ns);
                    }
                }
            }
		}

        private void UpdateList()
        {
            this.cbNames.BeginUpdate();
            try
            {
                this.cbNames.Items.Clear();

                foreach (NameRecord rec in this.fNames)
                {
                    string ns = rec.Name;
                    this.cbNames.Items.Add(new GKComboItem(ns, rec));

                    rec.ChIndex = -1;
                    ns = ns.ToUpper();

                    StringList lst = null;
                    if (rec.Sex == GEDCOMSex.svMale) {
                    	lst = this.fChurchMNames;
                    } else if (rec.Sex == GEDCOMSex.svFemale) {
                    	lst = this.fChurchFNames;
                    }

                    int num2 = lst.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        string st = lst[j];
                        if (st[0] == '-' && st.IndexOf(ns) >= 0)
                        {
                            rec.ChIndex = j;
                            break;
                        }
                    }
                }
            }
            finally
            {
                this.cbNames.EndUpdate();
            }
        }
	}
}
