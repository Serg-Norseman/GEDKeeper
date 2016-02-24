namespace GKCore.Interfaces
{
	/// <summary>
	/// 
	/// </summary>
	public interface IOptionsControl : ILocalization
	{
		IOptions Options { get; set; }
		
		void AcceptChanges();
		void UpdateControls();
	}
}
