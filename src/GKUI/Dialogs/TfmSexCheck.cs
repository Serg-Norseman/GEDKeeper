using System;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmSexCheck : Form
	{
		public TfmSexCheck()
		{
			this.InitializeComponent();
			this.btnAccept.Text = LangMan.LSList[97];
			this.btnCancel.Text = LangMan.LSList[98];
			this.Text = LangMan.LSList[104];
			this.GroupBox1.Text = LangMan.LSList[87];
			this.sbMale.Text = LangMan.LSList[66];
			this.sbFemale.Text = LangMan.LSList[67];
		}

		public static TGEDCOMSex DefineSex(string iName, string iPatr, NamesTable aNamesTable)
		{
			TGEDCOMSex sx = aNamesTable.GetSexByName(iName);
			TGEDCOMSex result = sx;

			if (sx == TGEDCOMSex.svNone)
			{
				TfmSexCheck dlg = new TfmSexCheck();
				try
				{
					if (dlg != null)
					{
						dlg.edName.Text = iName + " " + iPatr;
						sx = NamesTable.GetSex(iName, iPatr, false);

						if (sx != TGEDCOMSex.svNone)
						{
							if (sx == TGEDCOMSex.svMale)
							{
								dlg.sbMale.Checked = true;
								goto IL_78;
							}
							if (sx == TGEDCOMSex.svFemale)
							{
								dlg.sbFemale.Checked = true;
								goto IL_78;
							}
							if (sx != TGEDCOMSex.svUndetermined)
							{
								goto IL_78;
							}
						}
						dlg.sbNone.Checked = true;

						IL_78:
						if (dlg.ShowDialog() == DialogResult.OK)
						{
							if (dlg.sbNone.Checked)
							{
								sx = TGEDCOMSex.svNone;
							}
							else
							{
								if (dlg.sbMale.Checked)
								{
									sx = TGEDCOMSex.svMale;
								}
								else
								{
									if (dlg.sbFemale.Checked)
									{
										sx = TGEDCOMSex.svFemale;
									}
								}
							}
							result = sx;
							if (sx != TGEDCOMSex.svNone)
							{
								aNamesTable.SetNameSex(iName, sx);
							}
						}
					}
				}
				finally
				{
					dlg.Dispose();
				}
			}

			return result;
		}

		public static void CheckPersonSex(TGEDCOMIndividualRecord iRec, NamesTable aNamesTable)
		{
			if (iRec.Sex == TGEDCOMSex.svNone || iRec.Sex == TGEDCOMSex.svUndetermined)
			{
				string f_fam, f_name, f_patr;
				iRec.aux_GetNameParts(out f_fam, out f_name, out f_patr);
				iRec.Sex = DefineSex(f_name, f_patr, aNamesTable);
			}
		}

	}
}
