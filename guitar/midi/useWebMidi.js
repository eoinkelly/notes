// a react hook from kato that interfaces with web midi

import { useState, useEffect } from 'react';
import { useAppStore } from '../store';
import { isDevelop } from '../utils/system';
import { createChecksum } from '../utils/midi';
import CMDS from '../utils/cmds';
import cmds from '../utils/cmds';

export function useWebMidi() {
	const SYSEX_END = 0xf7;
	const QUERY_DATA = 0x11;
	const SET_DATA = 0x12;

	const PREFIX = [0xf0, 0x41, 0x00, 0x00, 0x00, 0x00, 0x33];

	const ADDR_EDIT_MODE = [0x7f, 0x00, 0x00, 0x01];
	const ADDR_CHANNEL_R = [0x00, 0x01, 0x00, 0x00];
	const ADDR_CHANNEL_W = [0x00, 0x01, 0x00, 0x00, 0x00];
	const ADDR_PREAMP_TYPE = [0x60, 0x00, 0x00, 0x21];
	const ADDR_PREAMP_BRIGHT = [0x60, 0x00, 0x00, 0x29];
	const ADDR_PREAMP_SOLO = [0x60, 0x0, 0x06, 0x14];
	const ADDR_FX_BOOSTER = [0x60, 0x00, 0x00, 0x10];
	const ADDR_FX_MOD = [0x60, 0x00, 0x01, 0x00];
	const ADDR_FX_FX = [0x60, 0x00, 0x03, 0x00];
	const ADDR_FX_DELAY = [0x60, 0x00, 0x05, 0x00];
	const ADDR_FX_REVERB = [0x60, 0x00, 0x05, 0x40];

	const offset = {
		type: 12,
		bright: 20,
	};

	const QUERY_SIZE_BYTE = 1;
	const QUERY_SIZE_WORD = 2;
	const QUERY_SIZE_12 = 12;

	const [restartMidi, setRestartMidi] = useState(false);

	const midi = useAppStore((state) => state.midi);
	const setMidi = useAppStore((state) => state.setMidi);
	const katMsg = useAppStore((state) => state.katMsg);
	const setKatMsg = useAppStore((state) => state.setKatMsg);
	const setMsgDialog = useAppStore((state) => state.setMsgDialog);
	const setPreampType = useAppStore((state) => state.setPreampType);
	const setBright = useAppStore((state) => state.setBright);
	const setSolo = useAppStore((state) => state.setSolo);
	const setBooster = useAppStore((state) => state.setBooster);
	const setMod = useAppStore((state) => state.setMod);
	const setFx = useAppStore((state) => state.setFx);
	const setDelay = useAppStore((state) => state.setDelay);
	const setReverb = useAppStore((state) => state.setReverb);
	const setChannel = useAppStore((state) => state.setChannel);

	const log = isDevelop();

	function showMsgMidiErr() {
		setMsgDialog({
			show: true,
			title: ' Communication Error',
			content:
				'Could not send MIDI data - please check the MIDI connection and refresh the browser',
		});
	}

	function onMIDIFailure(msg) {
		showMsgMidiErr();
	}

	//******************************************* */
	//*         Connect interfaces
	//*
	function onMIDISuccess(midiAccess) {
		for (let entry of midiAccess.inputs) {
			var input = entry[1];
			var inputPort;

			if (input.name.endsWith('KATANA') || input.name === 'KATANA MIDI 1') {
				inputPort = input;
			}
		}

		for (let entry of midiAccess.outputs) {
			var output = entry[1];
			var outputPort;

			if (output.name.endsWith('KATANA') || output.name === 'KATANA MIDI 1') {
				outputPort = output;
			}
		}

		setMidi({
			access: midiAccess,
			input: inputPort,
			output: outputPort,
		});

		// send EDIT_MODE when Midi connection restarts
		midiAccess.onstatechange = (event) => {
			if (event.port.state === 'connected') {
				setRestartMidi(!restartMidi);
			} else {
				showMsgMidiErr();
			}
		};
	}

	//******************************************* */
	//*     Filter incoming sysEx address
	function cmpArrays(addr, setter, event) {
		let a = new Uint8Array([...PREFIX, SET_DATA, ...addr]);
		let b = event.data.slice(0, 12);

		if (a.every((val, idx) => val === b[idx])) {
			setter(event.data[12]);
			return true;
		}

		return false;
	}

	//******************************************* */
	//*     Parse the incoming MIDI data
	//*
	function midiMessageEventHandler(event) {
		if (log) {
			var str = 'MIDI rcv ';
			for (var i = 0; i < event.data.length; i++) {
				str += '0x' + event.data[i].toString(16) + ' ';
			}

			console.log(str);
		}

		// ignore bulk data
		if (event.data.length === 255) {
			return;
		}

		// get preamp data
		if (event.data.length === 14 + QUERY_SIZE_12) {
			let a = new Uint8Array([...PREFIX, SET_DATA, ...ADDR_PREAMP_TYPE]);
			let b = event.data.slice(0, 12);

			if (a.every((val, idx) => val === b[idx])) {
				setPreampType(event.data[offset.type]);
				setBright(event.data[offset.bright]);
				return;
			}
		}

		// get channel
		if (event.data.length === 16) {
			let a = new Uint8Array([...PREFIX, SET_DATA, ...ADDR_CHANNEL_R]);
			let b = event.data.slice(0, 13);

			if (a.every((val, idx) => val === b[idx])) {
				setChannel(event.data[13]);
				return;
			}
		}

		// get solo and effects state
		if (event.data.length === 15) {
			if (cmpArrays(ADDR_PREAMP_SOLO, setSolo, event)) return;
			if (cmpArrays(ADDR_FX_BOOSTER, setBooster, event)) return;
			if (cmpArrays(ADDR_FX_MOD, setMod, event)) return;
			if (cmpArrays(ADDR_FX_FX, setFx, event)) return;
			if (cmpArrays(ADDR_FX_DELAY, setDelay, event)) return;
			if (cmpArrays(ADDR_FX_REVERB, setReverb, event)) return;

			return;
		}

		getStatus();
	}

	function getStatus() {
		requestData(CMDS.GET_PREAMP_DATA);
		requestData(CMDS.GET_PREAMP_SOLO);
		requestData(CMDS.GET_FX_BOOSTER);
		requestData(CMDS.GET_FX_MOD);
		requestData(CMDS.GET_FX_FX);
		requestData(CMDS.GET_FX_DELAY);
		requestData(CMDS.GET_FX_REVERB);
		requestData(cmds.GET_CHANNEL);
	}

	// let the amp process the last set-command
	function requestData(cmd) {
		setTimeout(() => {
			setKatMsg({
				cmd: cmd,
			});
		}, 50);
	}

	//******************************************* */
	//*         Send MIDI sysEx data
	//*
	function sendSysEx(write, address, data) {
		var checksum = write
			? createChecksum([...address, data])
			: createChecksum([...address, 0x00, 0x00, 0x00, data]);

		var msg = new Uint8Array([
			...PREFIX,
			write ? SET_DATA : QUERY_DATA,
			...checksum,
			SYSEX_END,
		]);

		try {
			midi.output.send(msg);
		} catch (error) {
			showMsgMidiErr();
		}
	}

	useEffect(() => {
		switch (katMsg.cmd) {
			case CMDS.EDIT_MODE:
				sendSysEx(true, ADDR_EDIT_MODE, katMsg.data);
				break;

			case CMDS.SET_CHANNEL:
				sendSysEx(true, ADDR_CHANNEL_W, katMsg.data);
				break;
			case CMDS.GET_CHANNEL:
				sendSysEx(false, ADDR_CHANNEL_R, QUERY_SIZE_WORD);
				break;

			case CMDS.SET_PREAMP_TYPE:
				sendSysEx(true, ADDR_PREAMP_TYPE, katMsg.data);
				requestData(CMDS.GET_PREAMP_DATA);
				break;

			case CMDS.SET_PREAMP_BRIGHT:
				sendSysEx(true, ADDR_PREAMP_BRIGHT, katMsg.data);
				requestData(CMDS.GET_PREAMP_DATA);
				break;

			case CMDS.SET_PREAMP_SOLO:
				sendSysEx(true, ADDR_PREAMP_SOLO, katMsg.data);
				requestData(CMDS.GET_PREAMP_SOLO);
				break;
			case CMDS.GET_PREAMP_SOLO:
				sendSysEx(false, ADDR_PREAMP_SOLO, QUERY_SIZE_BYTE);
				break;

			case CMDS.SET_FX_BOOSTER:
				sendSysEx(true, ADDR_FX_BOOSTER, katMsg.data);
				requestData(CMDS.GET_FX_BOOSTER);
				break;
			case CMDS.GET_FX_BOOSTER:
				sendSysEx(false, ADDR_FX_BOOSTER, QUERY_SIZE_BYTE);
				break;

			case CMDS.SET_FX_MOD:
				sendSysEx(true, ADDR_FX_MOD, katMsg.data);
				requestData(CMDS.GET_FX_MOD);
				break;
			case CMDS.GET_FX_MOD:
				sendSysEx(false, ADDR_FX_MOD, QUERY_SIZE_BYTE);
				break;

			case CMDS.SET_FX_FX:
				sendSysEx(true, ADDR_FX_FX, katMsg.data);
				requestData(CMDS.GET_FX_FX);
				break;
			case CMDS.GET_FX_FX:
				sendSysEx(false, ADDR_FX_FX, QUERY_SIZE_BYTE);
				break;

			case CMDS.SET_FX_DELAY:
				sendSysEx(true, ADDR_FX_DELAY, katMsg.data);
				requestData(CMDS.GET_FX_DELAY);
				break;
			case CMDS.GET_FX_DELAY:
				sendSysEx(false, ADDR_FX_DELAY, QUERY_SIZE_BYTE);
				break;

			case CMDS.SET_FX_REVERB:
				sendSysEx(true, ADDR_FX_REVERB, katMsg.data);
				requestData(CMDS.GET_FX_REVERB);
				break;
			case CMDS.GET_FX_REVERB:
				sendSysEx(false, ADDR_FX_REVERB, QUERY_SIZE_BYTE);
				break;

			case CMDS.GET_PREAMP_DATA:
				sendSysEx(false, ADDR_PREAMP_TYPE, QUERY_SIZE_12);
				break;
			default:
		}
		// eslint-disable-next-line
	}, [katMsg]);

	// enable the Katana edit mode and start the MIDI input listener
	useEffect(() => {
		if (!midi.input) return;

		setKatMsg({
			cmd: CMDS.EDIT_MODE,
			data: 0x01,
		});

		midi.input.onmidimessage = midiMessageEventHandler;

		getStatus();
		// eslint-disable-next-line
	}, [midi.input, restartMidi]);

	// request sysex access to MIDI interfaces
	useEffect(() => {
		navigator
			.requestMIDIAccess({ sysex: true })
			.then(onMIDISuccess, onMIDIFailure);
		// eslint-disable-next-line
	}, []);

	return;
}
