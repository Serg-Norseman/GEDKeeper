/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Utilities
{
    [TestFixture]
    public class SingleInstanceTrackerTests
    {
        private static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
        {
            return Substitute.For<ISingleInstanceEnforcer>();
        }

        [Test]
        public void Test_IpcMessage()
        {
            var ipcMsg = new IpcMessage();
            Assert.IsNotNull(ipcMsg);

            Assert.Throws(typeof(ArgumentNullException), () => { ipcMsg.Serialize(null); });

            Assert.Throws(typeof(ArgumentNullException), () => { IpcMessage.Deserialize(null); });
        }

        [Test]
        public void Test_IpcParamEx()
        {
            var ipcParam = new IpcParamEx();
            Assert.IsNotNull(ipcParam);
            Assert.AreEqual("", ipcParam.Message);
            Assert.AreEqual("", ipcParam.Params);

            ipcParam = new IpcParamEx("message", "params");
            Assert.IsNotNull(ipcParam);
            Assert.AreEqual("message", ipcParam.Message);
            Assert.AreEqual("params", ipcParam.Params);
        }

        [Test]
        public void Test_IpcFake()
        {
            IpcFake.StartServer(GetSingleInstanceEnforcer());

            Assert.AreEqual(true, IpcFake.CreateMutex("test", true));
            IpcFake.RefreshMutexes();

            Assert.AreEqual(false, IpcFake.CreateMutex("test", true));
            IpcFake.RefreshMutexes();

            Assert.Throws(typeof(ArgumentNullException), () => { IpcFake.SendMessage(null); });

            IpcFake.SendMessage(IpcFake.CmdSendArgs, new string[] { "testArgX" });

            IpcFake.StopServer();
            IpcFake.ReleaseAllMutexes();
        }

        [Test]
        public void Test_IpcFake_SafeSerialize()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { IpcFake.SafeSerialize(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { IpcFake.SafeDeserialize(null); });

            string[] args = new string[] { "testA", "testB" };
            string temp = IpcFake.SafeSerialize(args);
            string[] res = IpcFake.SafeDeserialize(temp);
            Assert.AreEqual(args[0], res[0]);
            Assert.AreEqual(args[1], res[1]);
        }

        [Test]
        public void Test_MessageEventArgs()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { new MessageEventArgs(null); });

            var args = new MessageEventArgs("test");
            Assert.IsNotNull(args);
        }

        [Test]
        public void Test_Common()
        {
            //Assert.Throws(typeof(ArgumentNullException), () => { new SingleInstanceProxy(null); });

            Assert.Throws(typeof(ArgumentNullException), () => { new SingleInstanceTracker(null, null); });

            string[] args = new string[1];

            using (SingleInstanceTracker tracker = new SingleInstanceTracker("test", GetSingleInstanceEnforcer)) {
                Assert.IsNotNull(tracker.Enforcer);

                if (tracker.IsFirstInstance) {
                } else {
                    tracker.SendMessageToFirstInstance(args);
                }
            }
        }
    }
}
