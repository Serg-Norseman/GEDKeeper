namespace GKCore.Interfaces
{
	public interface IWorkWindow
	{
		string GetStatusString();

		bool NavCanBackward();
		bool NavCanForward();
		void NavNext();
		void NavPrev();
	}
}
