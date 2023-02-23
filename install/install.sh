#!/bin/bash
cp manage-apps.service /etc/systemd/system/
systemctl daemon-reload
systemctl enable manage-apps.service
systemctl start manage-apps.service
