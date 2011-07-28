using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMTime : TGEDCOMTag
	{
		internal ushort FHour;
		internal ushort FMinutes;
		internal ushort FSeconds;
		internal ushort FFraction;

		[Browsable(false)]
		public ushort Fraction
		{
			get
			{
				return this.FFraction;
			}
			set
			{
				this.FFraction = value;
			}
		}
		[Browsable(false)]
		public ushort Hour
		{
			get
			{
				return this.FHour;
			}
			set
			{
				this.FHour = value;
			}
		}
		[Browsable(false)]
		public ushort Minutes
		{
			get
			{
				return this.FMinutes;
			}
			set
			{
				this.FMinutes = value;
			}
		}
		[Browsable(false)]
		public ushort Seconds
		{
			get
			{
				return this.FSeconds;
			}
			set
			{
				this.FSeconds = value;
			}
		}

		[Browsable(false)]
		public TimeSpan Time
		{
			get { return this.GetValue(); }
			set { this.SetValue(value); }
		}

		[Browsable(false)]
		public TimeSpan Value
		{
			get { return this.GetValue(); }
			set { this.SetValue(value); }
		}

		internal TimeSpan GetValue()
		{
			return new TimeSpan(0, (int)this.FHour, (int)this.FMinutes, (int)this.FSeconds, (int)(100u * (uint)this.FFraction));
		}
		internal void SetValue(TimeSpan AValue)
		{
			this.FHour = (ushort)AValue.Hours;
			this.FMinutes = (ushort)AValue.Minutes;
			this.FSeconds = (ushort)AValue.Seconds;
			ushort MSec = (ushort)AValue.Milliseconds;
			this.FFraction = (ushort)BDSSystem.Trunc(MSec / 100.0);
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "TIME";
		}
		protected internal override string GetStringValue()
		{
			string Result;
			if (this.FHour == 0 && this.FMinutes == 0 && this.FSeconds == 0)
			{
				Result = "";
			}
			else
			{
				Result = string.Format("{0:00}:{1:00}:{2:00}", new object[]
				{
					this.FHour, 
					this.FMinutes, 
					this.FSeconds
				});
				if (this.FFraction > 0)
				{
					Result = Result + "." + this.FFraction.ToString();
				}
			}
			return Result;
		}
		public override void Clear()
		{
			base.Clear();
			this.FHour = 0;
			this.FMinutes = 0;
			this.FSeconds = 0;
			this.FFraction = 0;
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.FHour == 0 && this.FMinutes == 0 && this.FSeconds == 0;
		}
		public override string ParseString([In] string AString)
		{
			this.FHour = 0;
			this.FMinutes = 0;
			this.FSeconds = 0;
			this.FFraction = 0;
			string Result = AString;
			if (BDSSystem.WStrCmp(Result, "") != 0)
			{
				Result = base.ExtractDelimiter(Result, 0);
				int H = 0;
				Result = TGEDCOMObject.ExtractNumber(Result, ref H, false, 0);
				this.FHour = (ushort)H;
				if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 1), ":") == 0)
				{
					Result = Result.Remove(0, 1);
				}
				int M = 0;
				Result = TGEDCOMObject.ExtractNumber(Result, ref M, false, 0);
				this.FMinutes = (ushort)M;
				if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 1), ":") == 0)
				{
					Result = Result.Remove(0, 1);
					int S = 0;
					Result = TGEDCOMObject.ExtractNumber(Result, ref S, false, 0);
					this.FSeconds = (ushort)S;
					if (BDSSystem.WStrCmp(BDSSystem.WStrCopy(Result, 1, 1), ".") == 0)
					{
						Result = Result.Remove(0, 1);
						int F = 0;
						Result = TGEDCOMObject.ExtractNumber(Result, ref F, false, 0);
						this.FFraction = (ushort)F;
					}
				}
			}
			return Result;
		}

		public TGEDCOMTime(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
