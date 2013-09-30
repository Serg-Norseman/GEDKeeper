using System;

namespace GedCom551
{
	public sealed class TGEDCOMTime : TGEDCOMTag
	{
		private ushort FHour;
		private ushort FMinutes;
		private ushort FSeconds;
		private ushort FFraction;

		public ushort Fraction
		{
			get { return this.FFraction; }
			set { this.FFraction = value; }
		}

		public ushort Hour
		{
			get { return this.FHour; }
			set { this.FHour = value; }
		}

		public ushort Minutes
		{
			get { return this.FMinutes; }
			set { this.FMinutes = value; }
		}

		public ushort Seconds
		{
			get { return this.FSeconds; }
			set { this.FSeconds = value; }
		}

		public TimeSpan Value
		{
			get {
				return new TimeSpan(0, (int)this.FHour, (int)this.FMinutes, (int)this.FSeconds, (int)(100u * (uint)this.FFraction));
			}
			set {
				this.FHour = (ushort)value.Hours;
				this.FMinutes = (ushort)value.Minutes;
				this.FSeconds = (ushort)value.Seconds;
				ushort MSec = (ushort)value.Milliseconds;
				this.FFraction = (ushort)Math.Truncate(MSec / 100.0);
			}
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "TIME";
		}

		protected override string GetStringValue()
		{
			string Result;
			if (this.FHour == 0 && this.FMinutes == 0 && this.FSeconds == 0)
			{
				Result = "";
			}
			else
			{
				Result = string.Format("{0:00}:{1:00}:{2:00}", new object[] { this.FHour, this.FMinutes, this.FSeconds });

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

		public override string ParseString(string AString)
		{
			this.FHour = 0;
			this.FMinutes = 0;
			this.FSeconds = 0;
			this.FFraction = 0;

			string result = AString;
			if (!string.IsNullOrEmpty(result))
			{
				result = GEDCOMUtils.ExtractDelimiter(result, 0);

				int tmp;
				result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
				this.FHour = (ushort)tmp;
				if (result != "" && result[0] == ':')
				{
					result = result.Remove(0, 1);
				}

				result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
				this.FMinutes = (ushort)tmp;
				if (result != "" && result[0] == ':')
				{
					result = result.Remove(0, 1);

					result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
					this.FSeconds = (ushort)tmp;
					if (result != "" && result[0] == '.')
					{
						result = result.Remove(0, 1);

						result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
						this.FFraction = (ushort)tmp;
					}
				}
			}
			return result;
		}

		public TGEDCOMTime(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMTime(owner, parent, tagName, tagValue);
		}
	}
}
