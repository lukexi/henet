/**
 @file  extra.h
 @brief type definitions for ENet
*/
#ifndef __ENET_EXTRA_H__
#define __ENET_EXTRA_H__

#include "enet/enet.h"

void
enet_host_broadcast_except (ENetHost * host, ENetPeer * exceptPeer, enet_uint8 channelID, ENetPacket * packet);

#endif /* __ENET_EXTRA_H__ */

