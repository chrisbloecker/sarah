#ifndef __DHT22_H__
#define __DHT22_H__

#include <stdio.h>
#include <pigpio.h>

// This is the only processor specific magic value, the maximum amount of time to
// spin in a loop before bailing out and considering the read a timeout.  This should
// be a high value, but if you're running on a much faster platform than a Raspberry
// Pi or Beaglebone Black then it might need to be increased.
#define DHT_MAXCOUNT 32000

// Number of bit pulses to expect from the DHT.  Note that this is 41 because
// the first pulse is a constant 50 microsecond pulse, with 40 pulses to represent
// the data afterwards.
#define DHT_PULSES 41

#define OK                (0)
#define ERROR_INIT_FAILED (1)
#define ERROR_TIMEOUT     (2)

static inline int readDHT22(uint32_t pin, double* humidity, double* temperature)
{
  // Init pigpio
  if (gpioInitialise() < 0)
  {
    // Initialization failed
    fprintf(stderr, "GPIO Initialization failed\n");
    return ERROR_INIT_FAILED;
  }

  unsigned i = 0;

  *humidity    = 0;
  *temperature = 0;

  // Store the count that each DHT bit pulse is low and high.
  // Make sure array is initialized to start at zero.
  int pulseCounts[DHT_PULSES*2] = {0};

  // Setup the GPIO pin as an output pin
  gpioSetMode(pin, PI_OUTPUT);

  gpioWrite(pin, 1);
  time_sleep(0.500);
  gpioWrite(pin, 0);
  time_sleep(0.020);

  gpioSetMode(pin, PI_INPUT);
  for (i = 0; i < 50; ++i);

  unsigned waiting;
  while (gpioRead(pin))
    if (++waiting >= DHT_MAXCOUNT)
    {
      fprintf(stderr, "Timeout reading DHT22\n");
      return ERROR_TIMEOUT;
    }

  for (i = 0; i < 2*DHT_PULSES; i += 2)
  {
    while (!gpioRead(pin))
      if (++pulseCounts[i] >= DHT_MAXCOUNT)
      {
        fprintf(stderr, "Timeout reading DHT22\n");
        return ERROR_TIMEOUT;
      }

    while (gpioRead(pin))
      if (++pulseCounts[i+1] >= DHT_MAXCOUNT)
      {
        fprintf(stderr, "Timeout reading DHT22\n");
        return ERROR_TIMEOUT;
      }
  }

  unsigned threshold = 0;
  for (i = 2; i < DHT_PULSES*2; i += 2)
    threshold += pulseCounts[i];
  threshold /= DHT_PULSES-1;

  uint8_t data[5] = {0};
  for (i = 3; i < DHT_PULSES*2; i += 2)
  {
    int index = (i-3) / 16;
    data[index] <<= 1;
    // One bit for long pulse.
    if (pulseCounts[i] >= threshold)
      data[index] |= 1;
  }

  // debug
  //fprintf(stderr, "Data: 0x%x 0x%x 0x%x 0x%x 0x%x\n", data[0], data[1], data[2], data[3], data[4]);

  // Verify checksum of received data.
  if (data[4] == ((data[0] + data[1] + data[2] + data[3]) & 0xFF))
  {
    *humidity    = (data[0] * 256 + data[1]) / 10.0;
    *temperature = ((data[2] & 0x7F) * 256 + data[3]) / 10.0;
    if (data[2] & 0x80)
      *temperature *= -1.0;
  }

  // Cleanup
  gpioTerminate();
  return OK;
}

#endif
