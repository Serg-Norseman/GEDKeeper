/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using NUnit.Framework;

namespace GKCore.SingleInstance
{
    [TestFixture]
    public class SingleInstanceTrackerTests
    {
        private class SingleInstanceEnforcerMock: ISingleInstanceEnforcer
        {
            public void OnMessageReceived(MessageEventArgs e) { }
            public void OnNewInstanceCreated(EventArgs e) { }
        }

        private static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
        {
            return new SingleInstanceEnforcerMock();
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
