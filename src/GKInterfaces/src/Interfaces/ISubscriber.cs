using GKCore.Types;

namespace GKCore.Interfaces
{
	public interface ISubscriber
	{
		void NotifyRecord(IBase aBase, object record, RecordAction action);
	}
}
