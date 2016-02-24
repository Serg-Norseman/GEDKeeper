namespace GKUI.Controls
{
	/// <summary>
	/// 
	/// </summary>
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
}
