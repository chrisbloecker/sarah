#ifndef __DHT22_H_
#define __DHT22_H_

#include <stdio.h>
#include <pigpio.h>

static inline int readDHT22(uint32_t pin, float* humidity, float* temperature)
{
  printf("Reading DHT22...\n");
  fflush(stdout);

  *humidity    = 80.0;
  *temperature = 42.0;

  return 0;
}

#endif
