class WayfarerSerial:

    def __init__(self, serial_port_path):
        self.callbacks = {}
        self.logger = logging.getLogger('radiola.etm.wayfarer')
        self.serial_port_path = serial_port_path
        self.logger.info("Connecting to {port}".format(port=serial_port_path))
        self.serial_port = serial.rs485.RS485(serial_port_path,
                                              baudrate=9600,
                                              # baudrate=19200,
                                              bytesize=serial.EIGHTBITS,
                                              parity=serial.PARITY_ODD,
                                              stopbits=serial.STOPBITS_TWO,
                                              timeout=1.0)
        self.serial_port.rs485_mode = serial.rs485.RS485Settings()

    def setCallback(self, event_type, callback):
        self.callbacks[event_type] = callback

    def listen(self):
        self.logger.debug("Listening to serial...")
        message_buffer = []
        while True:
            b = self.serial_port.read(1)
            # Did we get a byte?
            if b:
                message_buffer.append(ord(b))
                message_buffer = self.process_message_buffer(
                    message_buffer,
                    message_handler=self.handle)

    def handle(self, incoming_message):
        self.logger.debug("==================================================")
        self.logger.debug("Incoming message (hex) = {0}".format(
            hex_message(incoming_message.message)))
        # self.logger.debug("(HEX) = {0}".format(incoming_message.hex_message))

        self.incoming_message = incoming_message
        command = incoming_message.command_byte
        self.logger.info("Command {} = {}".format(
            command, EtmConstants.lookup_command_name(command)))

        try:
            incoming_message.verify_checksum()
        except StandardError, e:
            self.logger.exception(e)
            return

        if command == EtmConstants.REQUEST_STATUS:
            self.request_status()
        elif command == EtmConstants.ENTER_SERVICE:
            self.enter_service()  # This tells sinatra
...        elif command == EtmConstants.CASH_TOTAL:
            self.logger.info("CASH_TOTAL not implemented")
        else:
            self.logger.error("UNRECOGNISED COMMAND: {command_byte}".format(
                command_byte=command))

        # any registered callbacks? run them
        callback = self.callbacks.get(command)
        if callback:
            self.logger.debug("Callback found!")
            response = callback(incoming_message.decode())
            if response.data[""

        self.logger.debug("==================================================")
