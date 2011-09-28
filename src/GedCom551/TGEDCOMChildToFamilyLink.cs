using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMChildToFamilyLink : TGEDCOMPointerWithNotes
	{
		public TGEDCOMChildLinkageStatus ChildLinkageStatus
		{
			get { return this.GetChildLinkageStatus(); }
			set { this.SetChildLinkageStatus(value); }
		}

		public TGEDCOMPedigreeLinkageType PedigreeLinkageType
		{
			get { return this.GetPedigreeLinkageType(); }
			set { this.SetPedigreeLinkageType(value); }
		}

		public TGEDCOMFamilyRecord Family
		{
			get { return base.GetValue() as TGEDCOMFamilyRecord; }
			set { base.SetValue(value); }
		}

		private TGEDCOMPedigreeLinkageType GetPedigreeLinkageType()
		{
			string S = base.GetTagStringValue("PEDI").Trim().ToLower();
			TGEDCOMPedigreeLinkageType Result;

			if (S == "adopted")
			{
				Result = TGEDCOMPedigreeLinkageType.plAdopted;
			}
			else
			{
				if (S == "birth")
				{
					Result = TGEDCOMPedigreeLinkageType.plBirth;
				}
				else
				{
					if (S == "foster")
					{
						Result = TGEDCOMPedigreeLinkageType.plFoster;
					}
					else
					{
						if (S == "sealing")
						{
							Result = TGEDCOMPedigreeLinkageType.plSealing;
						}
						else
						{
							Result = TGEDCOMPedigreeLinkageType.plNone;
						}
					}
				}
			}
			return Result;
		}

		private void SetPedigreeLinkageType([In] TGEDCOMPedigreeLinkageType Value)
		{
			string S;
			if (Value != TGEDCOMPedigreeLinkageType.plAdopted)
			{
				if (Value != TGEDCOMPedigreeLinkageType.plBirth)
				{
					if (Value != TGEDCOMPedigreeLinkageType.plFoster)
					{
						if (Value != TGEDCOMPedigreeLinkageType.plSealing)
						{
							S = "";
						}
						else
						{
							S = "sealing";
						}
					}
					else
					{
						S = "foster";
					}
				}
				else
				{
					S = "birth";
				}
			}
			else
			{
				S = "adopted";
			}
			base.SetTagStringValue("PEDI", S);
		}

		private TGEDCOMChildLinkageStatus GetChildLinkageStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToLower();
			TGEDCOMChildLinkageStatus Result;

			if (S == "challenged")
			{
				Result = TGEDCOMChildLinkageStatus.clChallenged;
			}
			else
			{
				if (S == "disproven")
				{
					Result = TGEDCOMChildLinkageStatus.clDisproven;
				}
				else
				{
					if (S == "proven")
					{
						Result = TGEDCOMChildLinkageStatus.clProven;
					}
					else
					{
						Result = TGEDCOMChildLinkageStatus.clNone;
					}
				}
			}
			return Result;
		}

		private void SetChildLinkageStatus([In] TGEDCOMChildLinkageStatus Value)
		{
			string S;
			if (Value != TGEDCOMChildLinkageStatus.clChallenged)
			{
				if (Value != TGEDCOMChildLinkageStatus.clDisproven)
				{
					if (Value != TGEDCOMChildLinkageStatus.clProven)
					{
						S = "";
					}
					else
					{
						S = "proven";
					}
				}
				else
				{
					S = "disproven";
				}
			}
			else
			{
				S = "challenged";
			}
			base.SetTagStringValue("STAT", S);
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "FAMC";
		}

		public TGEDCOMChildToFamilyLink(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
